pub use super::CachedCerts;

#[tokio::test]
async fn google() {
    let mut certs = CachedCerts::new();

    let first = certs.refresh_if_needed().await.expect("failed");
    let second = certs.refresh_if_needed().await.expect("failed");
    assert_eq!(first, true);
    assert_eq!(second, false);
}
