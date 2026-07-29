use std::path::{Path, PathBuf};

use anyhow::{Context, ensure};
use arptypes::protocol::ImageType;
use tokio::io::AsyncWriteExt;
use uuid::Uuid;

const MAX_IMAGE_BYTES: usize = 20 * 1024 * 1024;

#[derive(Clone)]
pub struct ImageStore {
    directory: PathBuf,
    public_url: String,
    client: reqwest::Client,
}

pub struct StoredImage {
    pub id: String,
    pub path: PathBuf,
    pub public_url: String,
}

impl ImageStore {
    pub async fn new(data_dir: &Path, public_url: String) -> anyhow::Result<Self> {
        let directory = data_dir.join("images");
        tokio::fs::create_dir_all(&directory).await?;
        Ok(Self {
            directory,
            public_url: public_url.trim_end_matches('/').to_string(),
            client: reqwest::Client::new(),
        })
    }

    pub fn reserve(&self, _purpose: ImageType) -> StoredImage {
        let id = Uuid::new_v4().to_string();
        self.image(&id)
    }

    pub async fn upload_from_url(
        &self,
        source_url: &str,
        _purpose: ImageType,
    ) -> anyhow::Result<StoredImage> {
        let response = self
            .client
            .get(source_url)
            .send()
            .await?
            .error_for_status()?;
        if let Some(length) = response.content_length() {
            ensure!(
                length <= MAX_IMAGE_BYTES as u64,
                "image exceeds {MAX_IMAGE_BYTES} bytes"
            );
        }
        let content_type = response
            .headers()
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_string();
        let bytes = response.bytes().await?;
        ensure!(
            bytes.len() <= MAX_IMAGE_BYTES,
            "image exceeds {MAX_IMAGE_BYTES} bytes"
        );

        let image = self.reserve(_purpose);
        self.write(&image.id, &bytes, &content_type).await?;
        Ok(image)
    }

    pub async fn write(
        &self,
        id: &str,
        bytes: &[u8],
        content_type: &str,
    ) -> anyhow::Result<StoredImage> {
        ensure!(
            bytes.len() <= MAX_IMAGE_BYTES,
            "image exceeds {MAX_IMAGE_BYTES} bytes"
        );
        Uuid::parse_str(id).context("invalid image id")?;
        let image = self.image(id);
        let mut file = tokio::fs::File::create(&image.path).await?;
        file.write_all(bytes).await?;
        file.flush().await?;
        tokio::fs::write(self.content_type_path(id), content_type).await?;
        Ok(image)
    }

    pub async fn read(&self, id: &str) -> anyhow::Result<(Vec<u8>, String)> {
        Uuid::parse_str(id).context("invalid image id")?;
        let bytes = tokio::fs::read(self.path_for_id(id)).await?;
        let content_type = tokio::fs::read_to_string(self.content_type_path(id))
            .await
            .unwrap_or_else(|_| "application/octet-stream".to_string());
        Ok((bytes, content_type))
    }

    pub fn upload_url(&self, id: &str) -> String {
        format!("{}/api/images/{id}", self.public_url)
    }

    fn image(&self, id: &str) -> StoredImage {
        StoredImage {
            id: id.to_string(),
            path: self.path_for_id(id),
            public_url: format!("{}/images/{id}", self.public_url),
        }
    }

    fn path_for_id(&self, id: &str) -> PathBuf {
        self.directory.join(id)
    }

    fn content_type_path(&self, id: &str) -> PathBuf {
        self.directory.join(format!("{id}.content-type"))
    }
}
