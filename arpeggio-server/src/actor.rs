use std::collections::VecDeque;

use anyhow::anyhow;
use arpeggio::session::{DispatchAction, ImageOperation, SessionUser, dispatch_game_request};
use arptypes::{
    Game, GameLog,
    protocol::{GameIndex, GameRequest},
};
use serde_json::{Value, json};
use tokio::sync::{broadcast, mpsc, oneshot};

use crate::{images::ImageStore, storage::NativeStorage};

#[derive(Clone, Debug)]
pub struct GameChanged {
    pub game: Game,
    pub logs: Vec<(GameIndex, GameLog)>,
}

struct ActorRequest {
    user: SessionUser,
    request: GameRequest,
    response: oneshot::Sender<anyhow::Result<Value>>,
}

#[derive(Clone)]
pub struct GameActor {
    requests: mpsc::Sender<ActorRequest>,
    changes: broadcast::Sender<GameChanged>,
}

impl GameActor {
    pub fn spawn(mut storage: NativeStorage, images: ImageStore) -> Self {
        let (requests, mut request_receiver) = mpsc::channel::<ActorRequest>(128);
        let (changes, _) = broadcast::channel(128);
        let change_sender = changes.clone();

        tokio::spawn(async move {
            while let Some(message) = request_receiver.recv().await {
                let result = handle_request(
                    &mut storage,
                    &images,
                    &change_sender,
                    message.user,
                    message.request,
                )
                .await;
                let _ = message.response.send(result);
            }
        });

        Self { requests, changes }
    }

    pub async fn request(&self, user: SessionUser, request: GameRequest) -> anyhow::Result<Value> {
        let (response, receiver) = oneshot::channel();
        self.requests
            .send(ActorRequest {
                user,
                request,
                response,
            })
            .await
            .map_err(|_| anyhow!("game actor stopped"))?;
        receiver.await.map_err(|_| anyhow!("game actor stopped"))?
    }

    pub fn subscribe(&self) -> broadcast::Receiver<GameChanged> {
        self.changes.subscribe()
    }
}

async fn handle_request(
    storage: &mut NativeStorage,
    images: &ImageStore,
    changes: &broadcast::Sender<GameChanged>,
    user: SessionUser,
    request: GameRequest,
) -> anyhow::Result<Value> {
    let recent_logs = match &request {
        GameRequest::GMGetGame | GameRequest::PlayerGetGame => storage.recent_logs().await?,
        _ => VecDeque::new(),
    };
    let action = dispatch_game_request(
        storage.game(),
        storage.metadata(),
        &recent_logs,
        &user,
        request,
    )?;

    match action {
        DispatchAction::Respond(payload) => Ok(payload),
        DispatchAction::Change(changed) => match *changed {
            Ok(changed) => {
                let response_logs = changed.logs.clone();
                let indexed_logs = storage.update_game(changed).await?;
                publish(changes, storage.game(), indexed_logs);
                Ok(serde_json::to_value(Ok::<_, String>(response_logs))?)
            }
            Err(error) => Ok(serde_json::to_value(Err::<Vec<GameLog>, _>(format!(
                "{error:?}"
            )))?),
        },
        DispatchAction::Rollback(index) => {
            let game = storage.rollback(index).await?;
            publish(changes, &game, Vec::new());
            Ok(serde_json::to_value(Vec::<GameLog>::new())?)
        }
        DispatchAction::Image(ImageOperation::UploadFromUrl { url, purpose }) => {
            let image = images.upload_from_url(&url, purpose).await?;
            storage
                .register_image(&image.id, purpose, &image.path)
                .await?;
            Ok(json!({ "image_url": image.public_url }))
        }
        DispatchAction::Image(ImageOperation::RequestUpload { purpose }) => {
            let image = images.reserve(purpose);
            storage
                .register_image(&image.id, purpose, &image.path)
                .await?;
            Ok(json!({
                "upload_url": images.upload_url(&image.id),
                "final_url": image.public_url,
            }))
        }
    }
}

fn publish(changes: &broadcast::Sender<GameChanged>, game: &Game, logs: Vec<(GameIndex, GameLog)>) {
    let _ = changes.send(GameChanged {
        game: game.clone(),
        logs,
    });
}

#[cfg(test)]
mod tests {
    use arptypes::{GMCommand, PlayerID};

    use super::*;

    #[tokio::test]
    async fn serializes_game_changes_through_one_actor() {
        let directory = tempfile::tempdir().unwrap();
        let storage = NativeStorage::open(directory.path()).await.unwrap();
        let images = ImageStore::new(directory.path(), "http://server.test".to_string())
            .await
            .unwrap();
        let actor = GameActor::spawn(storage, images);
        let mut changes = actor.subscribe();
        let player_id = PlayerID("Alice".to_string());

        let payload = actor
            .request(
                SessionUser::gm(),
                GameRequest::GMCommand {
                    command: Box::new(GMCommand::RegisterPlayer {
                        id: player_id.clone(),
                    }),
                },
            )
            .await
            .unwrap();
        let result: Result<Vec<GameLog>, String> = serde_json::from_value(payload).unwrap();
        assert_eq!(result.unwrap().len(), 1);
        assert!(
            changes
                .recv()
                .await
                .unwrap()
                .game
                .players
                .contains_key(&player_id)
        );

        let payload = actor
            .request(SessionUser::player(player_id), GameRequest::PlayerGetGame)
            .await
            .unwrap();
        assert!(payload.get("game").is_some());
    }
}
