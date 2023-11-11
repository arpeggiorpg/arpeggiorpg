//! Functions for interacting with the CloudFlare Images API.

use anyhow::anyhow;
use mtarp::types::GameID;
use serde_json::json;
use tracing::info;
use uuid::Uuid;
use worker::Url;

// Maybe we could make this implement a Trait, and then also implement a version of this that knows
// how to store images for local development.
pub struct CFImageService {
  account_id: String,
  image_delivery_prefix: Url,
  game_id: GameID,
}

impl CFImageService {
  pub fn new(
    account_id: String, image_delivery_prefix: &str, game_id: GameID,
  ) -> anyhow::Result<CFImageService> {
    let image_delivery_prefix = Url::parse(image_delivery_prefix)?;
    Ok(CFImageService { account_id, image_delivery_prefix, game_id })
  }

  pub async fn upload_from_url(
    &self, url: &str, purpose: mtarp::types::ImageType,
  ) -> anyhow::Result<Url> {
    // curl --request POST \
    // --url https://api.cloudflare.com/client/v4/accounts/<ACCOUNT_ID>/images/v1 \
    // --header 'Authorization: Bearer <API_TOKEN>' \
    // --form 'url=https://[user:password@]example.com/<PATH_TO_IMAGE>' \
    // --form 'metadata={"key":"value"}' \
    // --form 'requireSignedURLs=false'

    let api_url =
      format!("https://api.cloudflare.com/client/v4/accounts/{}/images/v1", self.account_id);
    let metadata = serde_json::to_string(&json!({"purpose": purpose.to_string()}))?;
    let params = [("url", url), ("metadata", &metadata), ("id", &self.gen_custom_id())];
    let client = reqwest::Client::new();
    let response = client.post(api_url).form(&params).send().await?;
    let response = response.error_for_status()?;

    let response = response.json::<serde_json::Value>().await?;
    if response.get("success") != Some(&serde_json::Value::Bool(true)) {
      info!(event = "bad-url-upload", ?response);
      return Err(anyhow!("Upload not successful"));
    }
    self.get_final_url(response)
  }

  fn gen_custom_id(&self) -> String { format!("{}/{}", self.game_id, Uuid::new_v4()) }

  pub async fn request_upload_image(
    &self, purpose: mtarp::types::ImageType,
  ) -> anyhow::Result<PendingImage> {
    let api_url = format!(
      "https://api.cloudflare.com/client/v4/accounts/{}/images/v2/direct_upload",
      self.account_id
    );
    let client = reqwest::Client::new();
    let metadata = serde_json::to_string(&json!({"purpose": purpose.to_string()}))?;
    let params = [("id", &self.gen_custom_id()), ("metadata", &metadata)];
    let response = client.post(api_url).form(&params).send().await?;
    let response = response.error_for_status()?;
    let response = response.json::<serde_json::Value>().await?;

    if response.get("success") != Some(&serde_json::Value::Bool(true)) {
      return Err(anyhow!("Upload not successful"));
    }

    let presigned_url = response
      .get("result")
      .ok_or(anyhow!("Couldn't find result field in image API result"))?
      .get("uploadURL")
      .ok_or(anyhow!("Couldn't find uploadURL in image API result"))?
      .as_str()
      .ok_or(anyhow!("uploadURL is not a string"))?;

    let presigned_url = Url::parse(presigned_url)?;
    Ok(PendingImage { upload_url: presigned_url, final_url: self.get_final_url(response)? })
  }

  fn get_final_url(&self, response: serde_json::Value) -> anyhow::Result<Url> {
    let image_id = response
      .get("result")
      .ok_or(anyhow!("Couldn't find result field in image API result"))?
      .get("id")
      .ok_or(anyhow!("Couldn't find image ID after uploading"))?
      .as_str()
      .ok_or(anyhow!("image ID is not a string"))?;
    let response_url = self.image_delivery_prefix.join(&self.account_id)?.join(image_id)?;
    Ok(response_url)
  }
}

#[derive(Clone, Debug)]
pub struct PendingImage {
  pub upload_url: Url,
  pub final_url: Url,
}
