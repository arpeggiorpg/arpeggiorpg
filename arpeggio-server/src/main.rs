use std::{net::SocketAddr, path::PathBuf};

use arpeggio_server::{ServerConfig, app};
use clap::Parser;
use tracing::info;
use tracing_subscriber::EnvFilter;

#[derive(Debug, Parser)]
#[command(about = "Run one standalone Arpeggio game")]
struct Arguments {
    #[arg(long, default_value = "127.0.0.1:3000")]
    listen: SocketAddr,

    #[arg(long, default_value = "./arpeggio-data")]
    data_dir: PathBuf,

    /// Browser-visible URL for this server, used in generated image URLs.
    #[arg(long)]
    public_url: Option<String>,

    /// UI origins allowed to call HTTP endpoints and open WebSockets.
    #[arg(
        long,
        value_delimiter = ',',
        default_value = "http://127.0.0.1:8080,http://localhost:8080"
    )]
    allowed_origin: Vec<String>,

    /// Optionally serve a compiled Dioxus web bundle, with SPA fallback to index.html.
    #[arg(long)]
    ui_dir: Option<PathBuf>,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();
    let arguments = Arguments::parse();
    let public_url = arguments
        .public_url
        .unwrap_or_else(|| format!("http://{}", arguments.listen));
    let router = app(ServerConfig {
        data_dir: arguments.data_dir,
        public_url,
        allowed_origins: arguments.allowed_origin,
        ui_dir: arguments.ui_dir,
    })
    .await?;
    let listener = tokio::net::TcpListener::bind(arguments.listen).await?;
    info!(address = %listener.local_addr()?, "Arpeggio server listening");
    axum::serve(listener, router).await?;
    Ok(())
}
