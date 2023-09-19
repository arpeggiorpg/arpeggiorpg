use std::path::Path;
use std::fs;
use log::error;

use anyhow::{Error};

use tonic::{Request, Response, Status};
tonic::include_proto!("pandt");

pub use pt_server::{Pt, PtServer};

use crate::actor::AppActor;

pub struct Server {
  actor: AppActor
}


impl Server {
  pub fn new(actor: AppActor) -> Server {
    Server { actor }
  }
}

#[tonic::async_trait]
impl Pt for Server {
  async fn say_hello(
    &self,
    request: Request<HelloRequest>, // Accept request of type HelloRequest
  ) -> Result<Response<HelloReply>, Status> {
    // Return an instance of type HelloReply
    // We must use .into_inner() as the fields of gRPC requests and responses are private
    let x = request.into_inner();
    println!("Got a request: {:?}", x);

    let reply = HelloReply {
      message: format!("Hello {}!", x.name).into(),
    };

    Ok(Response::new(reply)) // Send back our formatted greeting
  }


  async fn list_saved_games(&self, _request: Request<Empty>) -> Result<Response<ListSavedGamesReply>, Status> {
    // This does not require access to the app, so we don't dispatch to the actor.

    fn list_dir_into_strings(path: &Path) -> Result<Vec<String>, Error> {
      let mut result = vec![];
      for mpath in fs::read_dir(path)? {
        let path = mpath?;
        if path.file_type()?.is_file() {
          match path.file_name().into_string() {
            Ok(s) => result.push(s),
            Err(x) => error!("Couldn't parse filename as unicode: {:?}", x),
          }
        }
      }
      Ok(result)
    }

    let modules = match self.actor.module_path {
      Some(ref path) => list_dir_into_strings(path.as_ref()).map_err(map_err)?,
      None => vec![],
    };
    let games = list_dir_into_strings(&self.actor.saved_game_path).map_err(map_err)?;
    return Ok(Response::new(ListSavedGamesReply { modules, games }));
  }

  async fn save_game(&self, request: Request<SaveGameRequest>) -> Result<Response<Empty>, Status> {
    self.actor.save_game(request.into_inner().name).await.map_err(map_err)?;
    return Ok(Response::new(Empty {}));
  }
}

fn map_err<T: ToString>(e: T) -> Status {
  return Status::unknown(e.to_string());
}
