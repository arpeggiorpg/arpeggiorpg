use std::sync::mpsc;
use std::thread;

#[derive(Clone)]
pub struct Actor<Req, Resp> {
  request_sender: mpsc::Sender<ActorMsg<Req, Resp>>,
}

enum ActorMsg<Req, Resp> {
  Payload(Req, mpsc::Sender<Resp>),
  Stop,
}

impl<Req, Resp> Actor<Req, Resp>
where
  Req: Send + 'static,
  Resp: Send + 'static,
{
  pub fn spawn<Init, S, Handler>(init: Init, mut handler: Handler) -> Self
  where
    Init: FnOnce() -> S,
    Init: Send + 'static,
    Handler: FnMut(&mut S, Req) -> Resp,
    Handler: Send + 'static,
  {
    let (request_sender, request_receiver) = mpsc::channel();
    let actor = Actor { request_sender };
    thread::spawn(move || {
      let mut state = init();
      loop {
        let request = request_receiver.recv().unwrap();
        match request {
          ActorMsg::Payload(r, sender) => sender.send(handler(&mut state, r)).unwrap(),
          ActorMsg::Stop => break,
        }
      }
    });
    actor
  }

  pub fn send(&self, message: Req) -> Resp {
    let (response_sender, response_receiver) = mpsc::channel();
    self
      .request_sender
      .send(ActorMsg::Payload(message, response_sender))
      .unwrap();
    response_receiver.recv().unwrap()
  }

  pub fn stop(&self) { self.request_sender.send(ActorMsg::Stop).unwrap(); }
}
