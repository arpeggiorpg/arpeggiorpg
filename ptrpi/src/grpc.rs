use tonic::{Request, Response, Status};
tonic::include_proto!("pandt");

pub use pt_server::{Pt, PtServer};

#[derive(Debug, Default)]
pub struct Server {}

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
}
