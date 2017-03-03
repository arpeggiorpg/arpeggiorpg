use std::collections::HashSet;
use rocket::response::{self, Response, Responder};
use rocket::http::Method;

pub struct CORS<R> {
  responder: R,
  allow_origin: &'static str,
  expose_headers: HashSet<&'static str>,
  allow_credentials: bool,
  allow_headers: HashSet<&'static str>,
  allow_methods: HashSet<Method>,
  max_age: Option<usize>,
}

pub type PreflightCORS = CORS<()>;

impl PreflightCORS {
  pub fn preflight(origin: &'static str) -> PreflightCORS {
    CORS::origin((), origin)
  }
}

impl<'r, R: Responder<'r>> CORS<R> {
  pub fn origin(responder: R, origin: &'static str) -> CORS<R> {
    CORS {
      responder: responder,
      allow_origin: origin,
      expose_headers: HashSet::new(),
      allow_credentials: false,
      allow_headers: HashSet::new(),
      allow_methods: HashSet::new(),
      max_age: None,
    }
  }

  pub fn any(responder: R) -> CORS<R> {
    CORS::origin(responder, "*")
  }

  pub fn credentials(mut self, value: bool) -> CORS<R> {
    self.allow_credentials = value;
    self
  }

  pub fn methods(mut self, methods: Vec<Method>) -> CORS<R> {
    for method in methods {
      self.allow_methods.insert(method);
    }

    self
  }

  pub fn headers(mut self, headers: Vec<&'static str>) -> CORS<R> {
    for header in headers {
      self.allow_headers.insert(header);
    }

    self
  }

  // TODO: Add more builder methods to set the rest of the fields.
}

impl<'r, R: Responder<'r>> Responder<'r> for CORS<R> {
  fn respond(self) -> response::Result<'r> {
    let mut response = Response::build_from(self.responder.respond()?)
      .raw_header("Access-Control-Allow-Origin", self.allow_origin)
      .finalize();

    match self.allow_credentials {
      true => response.set_raw_header("Access-Control-Allow-Credentials", "true"),
      false => response.set_raw_header("Access-Control-Allow-Credentials", "false"),
    };

    if !self.allow_methods.is_empty() {
      let mut methods = String::with_capacity(self.allow_methods.len() * 7);
      for (i, method) in self.allow_methods.iter().enumerate() {
        if i != 0 {
          methods.push_str(", ")
        }
        methods.push_str(method.as_str());
      }

      response.set_raw_header("Access-Control-Allow-Methods", methods);
    }

    // FIXME: Get rid of this dupe.
    if !self.allow_headers.is_empty() {
      let mut headers = String::with_capacity(self.allow_headers.len() * 15);
      for (i, header) in self.allow_headers.iter().enumerate() {
        if i != 0 {
          headers.push_str(", ")
        }
        headers.push_str(header);
      }

      response.set_raw_header("Access-Control-Allow-Headers", headers);
    }

    // TODO: Inspect and set the rest of the fields.

    Ok(response)
  }
}
