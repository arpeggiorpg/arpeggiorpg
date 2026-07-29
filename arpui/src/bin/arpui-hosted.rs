fn main() {
    dioxus_logger::init(tracing::Level::INFO).expect("failed to init logger");
    dioxus::launch(arpui::hosted_app::App);
}
