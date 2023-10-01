use tonic::{Request, Response, Status};
mod rpc {
  tonic::include_proto!("pandt");
}

pub use rpc::pt_server::{Pt, PtServer};

use crate::actor::AppActor;
use pandt::types as T;

pub struct Server {
  actor: AppActor,
}

impl Server {
  pub fn new(actor: AppActor) -> Server { Server { actor } }
}

type RPCResult<T> = Result<Response<T>, Status>;

#[tonic::async_trait]
impl Pt for Server {
  async fn preview_volume_targets(
    &self, request: Request<rpc::PreviewVolumeTargetsRequest>,
  ) -> RPCResult<rpc::PreviewVolumeTargetsReply> {
    let request = request.into_inner();
    let (creatures, points) = self
      .actor
      .preview_volume_targets(
        T::SceneID(request.scene_id.parse().unwrap()),
        T::CreatureID(request.actor_id.parse().unwrap()),
        T::AbilityID(request.ability_id.parse().unwrap()),
        request.point.unwrap().into(),
      )
      .await
      .map_err(map_err)?;
    let points = points.into_iter().map(Into::into).collect();
    let creatures = creatures.into_iter().map(|cid| cid.0.to_string()).collect();
    Ok(Response::new(rpc::PreviewVolumeTargetsReply { creatures, points }))
  }

  async fn say_hello(
    &self,
    request: Request<rpc::HelloRequest>, // Accept request of type HelloRequest
  ) -> RPCResult<rpc::HelloReply> {
    // Return an instance of type HelloReply
    // We must use .into_inner() as the fields of gRPC requests and responses are private
    let x = request.into_inner();
    println!("Got a request: {:?}", x);

    let reply = rpc::HelloReply { message: format!("Hello {}!", x.name).into() };

    Ok(Response::new(reply)) // Send back our formatted greeting
  }

  async fn list_saved_games(
    &self, _request: Request<rpc::Empty>,
  ) -> RPCResult<rpc::ListSavedGamesReply> {
    let (modules, games) = self.actor.list_saved_games().await.map_err(map_err)?;
    return Ok(Response::new(rpc::ListSavedGamesReply { modules, games }));
  }

  async fn save_game(&self, request: Request<rpc::SaveGameRequest>) -> RPCResult<rpc::Empty> {
    self.actor.save_game(request.into_inner().name).await.map_err(map_err)?;
    return Ok(Response::new(rpc::Empty {}));
  }
}

fn map_err<T: ToString>(e: T) -> Status { return Status::unknown(e.to_string()); }

// Conversions!

impl From<T::Point3> for rpc::Point3 {
  fn from(p: T::Point3) -> rpc::Point3 {
    use uom::si::length::meter;
    return rpc::Point3 { x: p.x.get::<meter>(), y: p.y.get::<meter>(), z: p.z.get::<meter>() };
  }
}

impl From<rpc::Point3> for T::Point3 {
  fn from(p: rpc::Point3) -> T::Point3 { return T::Point3::new(p.x, p.y, p.z); }
}
