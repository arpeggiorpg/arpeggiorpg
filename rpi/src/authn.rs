use std::sync::Arc;
use std::time::Duration;

use anyhow::{self, Context};
use tokio::sync::Mutex;
use tracing::debug;

use mtarp::storage::Storage;
use mtarp::actor::AuthenticatedService;
use mtarp::types::UserID;

use crate::waiters::PingService;


#[derive(thiserror::Error, Debug)]
#[error("Authentication Error")]
pub struct AuthenticationError {
  #[from]
  pub from: anyhow::Error,
}

/// AuthenticatableService is a capability layer that hands out AuthenticatedServices to users who
/// authenticate.
#[derive(Clone)]
pub struct AuthenticatableService {
  pub storage: Arc<dyn Storage>,

  ping_service: Arc<PingService>,

  /// This is google client ID
  pub google_client_id: String,
  /// Cached certs for use by google_signin
  pub cached_certs: Arc<Mutex<google_signin::CachedCerts>>,
}

impl AuthenticatableService {
  pub fn new(storage: Arc<dyn Storage>, google_client_id: String) -> AuthenticatableService {
    AuthenticatableService {
      storage,
      google_client_id,
      cached_certs: Arc::new(Mutex::new(google_signin::CachedCerts::new())),
      ping_service: Arc::new(PingService::new()),
    }
  }

  /// Verify a google ID token and return an AuthenticatedService if it's valid.
  pub async fn authenticate(
    &self, google_id_token: String,
  ) -> Result<AuthenticatedService, AuthenticationError> {
    let user_id = self
      .validate_google_token(&google_id_token)
      .await
      .context("Validating Google ID Token".to_string())
      .map_err(|e| AuthenticationError { from: e })?;
    Ok(AuthenticatedService {
      user_id,
      storage: self.storage.clone(),
    })
  }

  async fn validate_google_token(&self, id_token: &str) -> anyhow::Result<UserID> {
    let mut certs = self.cached_certs.lock().await;
    certs.refresh_if_needed().await?;
    let mut client = google_signin::Client::new();
    client.audiences.push(self.google_client_id.clone());
    let id_info = client.verify(id_token, &certs).await?;
    let expiry = std::time::UNIX_EPOCH + Duration::from_secs(id_info.exp);
    let time_until_expiry = expiry.duration_since(std::time::SystemTime::now());
    debug!(
      "validate-token: email={:?} name={:?} sub={:?} expires={:?} expires IN: {:?}",
      id_info.email, id_info.name, id_info.sub, id_info.exp, time_until_expiry
    );
    Ok(UserID(format!("google_{}", id_info.sub)))
  }
}
