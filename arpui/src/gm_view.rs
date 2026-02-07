use std::collections::VecDeque;

use arptypes::{
    Folder, GMCommand, Game, GameLog, ModuleSource, SerializedGame,
    multitenant::{GameAndMetadata, GameID, GameIndex, InvitationID, RPIGameRequest, Role},
};
use dioxus::prelude::*;
use foldertree::FolderPath;
use tracing::{error, info};

use crate::{
    PLAYER_SPEC, PlayerSpec,
    components::button::{Button, ButtonVariant},
    player_view::GAME_NAME,
    rpi::{Connector, send_request, use_ws},
};

pub static GM_GAME: GlobalSignal<Game> = Signal::global(|| Default::default());
pub static GM_GAME_LOGS: GlobalSignal<VecDeque<(GameIndex, GameLog)>> =
    Signal::global(|| VecDeque::new());

#[component]
pub fn GMGamePage(id: GameID) -> Element {
    use_effect(move || *PLAYER_SPEC.write() = Some(PlayerSpec::GM));
    rsx! {
        Connector {
            role: Role::GM,
            game_id: id,
            game_signal: Some(GM_GAME.resolve()),
            player_game_signal: None,
            game_logs_signal: GM_GAME_LOGS.resolve(),

            GameLoader { game_id: id }
        }
    }
}

#[component]
fn GameLoader(game_id: GameID) -> Element {
    let ws = use_ws();
    let future: Resource<anyhow::Result<Game>> = use_resource(move || async move {
        info!("fetching game state for GM view");
        let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
        let game = Game::from_serialized_game(response.game);
        *GM_GAME.write() = game.clone();
        *GM_GAME_LOGS.write() = response.logs;
        *GAME_NAME.write() = response.metadata.name.clone();
        Ok(game)
    });

    match &*future.read_unchecked() {
        Some(Ok(game)) => {
            rsx! {
                Shell { game: game.clone(), game_id }
            }
        }
        Some(Err(err)) => {
            error!("GM view failed to load game state: {:?}", err);
            rsx! { div { "Unable to load GM view." } }
        }
        None => {
            rsx! { div { "Loading GM view..." } }
        }
    }
}

#[component]
fn Shell(game: Game, game_id: GameID) -> Element {
    let game = GM_GAME();
    let num_scenes = game.scenes.len();
    let num_creatures = game.creatures.len();
    let num_classes = game.classes.len();
    let active_scene_name = game
        .active_scene
        .and_then(|sid| game.scenes.get(&sid))
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "None".to_string());

    rsx! {
        div {
            class: "flex flex-col gap-4 p-6",
            div {
                class: "bg-white rounded-lg shadow-md p-4",
                h2 {
                    class: "text-lg font-semibold text-gray-800 mb-3",
                    "Game Overview"
                }
                div {
                    class: "space-y-2 text-sm text-gray-700",
                    p { "Active Scene: {active_scene_name}" }
                    p { "Scenes: {num_scenes}" }
                    p { "Creatures: {num_creatures}" }
                    p { "Classes: {num_classes}" }
                    if game.current_combat.is_some() {
                        p {
                            class: "font-medium text-blue-700",
                            "Combat is active"
                        }
                    }
                }
            }

            CampaignTreeCard {}

            Invitations { game_id }
        }
    }
}

#[component]
fn CampaignTreeCard() -> Element {
    rsx! {
        div {
            class: "bg-white rounded-lg shadow-md p-4",
            h2 {
                class: "text-lg font-semibold text-gray-800 mb-1",
                "Campaign"
            }
            p {
                class: "text-sm text-gray-500 mb-3",
                "Folder tree with scenes, creatures, notes, classes, abilities, and items."
            }
            CampaignFolder {
                path: FolderPath::root(),
                display_name: "Campaign".to_string(),
                depth: 0,
                start_open: true,
            }
        }
    }
}

#[component]
fn CampaignFolder(path: FolderPath, display_name: String, depth: usize, start_open: bool) -> Element {
    let game = GM_GAME();
    let mut is_expanded = use_signal(move || start_open);
    let mut show_menu = use_signal(|| false);
    let mut show_import_modal = use_signal(|| false);

    let folder = match game.campaign.get(&path) {
        Ok(folder) => folder,
        Err(_) => {
            return rsx! {
                div {
                    class: "text-sm text-red-600",
                    "Missing folder at {path}"
                }
            };
        }
    };

    let mut child_names: Vec<String> = match game.campaign.get_children(&path) {
        Ok(children) => children.iter().cloned().collect(),
        Err(_) => Vec::new(),
    };
    child_names.sort();

    let note_names = sorted_note_names(folder);
    let class_names = sorted_class_names(&game, folder);
    let ability_names = sorted_ability_names(&game, folder);
    let scene_names = sorted_scene_names(&game, folder);
    let creature_names = sorted_creature_names(&game, folder);
    let item_names = sorted_item_names(&game, folder);

    let padding_style = format!("margin-left: {}rem;", depth as f32 * 0.85);
    let path_key = if path.is_root() {
        "/".to_string()
    } else {
        path.to_string()
    };

    rsx! {
        div {
            class: "space-y-1",
            style: "{padding_style}",

            div {
                class: "relative flex items-center gap-2 rounded border border-gray-200 bg-gray-50 px-3 py-2",
                button {
                    class: "flex-1 text-left hover:bg-gray-100",
                    onclick: move |_| is_expanded.toggle(),
                    div {
                        class: "flex items-center gap-2",
                        span {
                            class: "text-xs text-gray-500",
                            if is_expanded() { "v" } else { ">" }
                        }
                        span { class: "text-sm font-medium text-gray-800", "Folder: {display_name}" }
                    }
                }

                button {
                    class: "rounded px-2 py-1 text-sm text-gray-600 hover:bg-gray-200",
                    onclick: move |evt: Event<MouseData>| {
                        evt.stop_propagation();
                        show_menu.toggle();
                    },
                    "..."
                }

                if show_menu() {
                    div {
                        class: "absolute right-0 top-full z-10 mt-1 w-48 rounded border border-gray-200 bg-white py-1 shadow-lg",
                        button {
                            class: "block w-full px-3 py-2 text-left text-sm text-gray-700 hover:bg-gray-100",
                            onclick: move |evt: Event<MouseData>| {
                                evt.stop_propagation();
                                show_menu.set(false);
                                show_import_modal.set(true);
                            },
                            "Import Module Here"
                        }
                    }
                }
            }

            if show_import_modal() {
                ImportModuleModal {
                    path: path.clone(),
                    on_close: move |_| show_import_modal.set(false),
                }
            }

            if is_expanded() {
                div {
                    class: "ml-2 space-y-2 border-l border-gray-200 pl-3 pb-1",

                    FolderContentSection {
                        title: "Notes",
                        icon: "[N]",
                        names: note_names,
                    }
                    FolderContentSection {
                        title: "Classes",
                        icon: "[C]",
                        names: class_names,
                    }
                    FolderContentSection {
                        title: "Abilities",
                        icon: "[A]",
                        names: ability_names,
                    }
                    FolderContentSection {
                        title: "Scenes",
                        icon: "[S]",
                        names: scene_names,
                    }
                    FolderContentSection {
                        title: "Creatures",
                        icon: "[R]",
                        names: creature_names,
                    }
                    FolderContentSection {
                        title: "Items",
                        icon: "[I]",
                        names: item_names,
                    }

                    if !child_names.is_empty() {
                        div {
                            class: "pt-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                            "Folders"
                        }
                    }

                    div {
                        class: "space-y-1",
                        for child_name in child_names {
                            CampaignFolder {
                                key: "{path_key}/{child_name}",
                                path: path.child(child_name.clone()),
                                display_name: child_name,
                                depth: depth + 1,
                                start_open: false,
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn ImportModuleModal(path: FolderPath, on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let destination = folder_path_label(&path);
    let mut import_action = use_action({
        let ws = ws.clone();
        let path = path.clone();
        move |file: dioxus::html::FileData| {
            let ws = ws.clone();
            let path = path.clone();
            async move {
                let file_name = file.name();
                let file_contents = file
                    .read_string()
                    .await
                    .map_err(|e| anyhow::anyhow!("Failed to read file '{file_name}': {e}"))?;
                let module = parse_module_file_contents(&file_contents)?;
                let command = GMCommand::LoadModule {
                    name: file_name.clone(),
                    source: ModuleSource::Module,
                    game: module,
                    path: path.child(file_name.clone()),
                };
                send_request::<serde_json::Value>(
                    RPIGameRequest::GMCommand {
                        command: Box::new(command),
                    },
                    ws,
                )
                .await?;
                Ok::<String, anyhow::Error>(file_name)
            }
        }
    });

    rsx! {
        crate::components::modal::Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            h3 {
                class: "text-lg font-semibold text-gray-800 mb-2",
                "Import Module Here"
            }
            p {
                class: "text-sm text-gray-600 mb-4",
                "Destination folder: {destination}"
            }
            input {
                class: "block w-full cursor-pointer rounded border border-gray-300 bg-white px-3 py-2 text-sm text-gray-700",
                r#type: "file",
                accept: ".arpeggiogame,.json",
                disabled: import_action.pending(),
                onchange: move |evt: FormEvent| {
                    if let Some(file) = evt.files().into_iter().next() {
                        import_action.call(file);
                    }
                },
            }
            if import_action.pending() {
                p {
                    class: "mt-2 text-sm text-gray-500",
                    "Importing module..."
                }
            }
            match import_action.value() {
                Some(Err(err)) => rsx! {
                    p {
                        class: "mt-2 text-sm text-red-600",
                        "Import failed: {err}"
                    }
                },
                Some(Ok(name)) => rsx! {
                    p {
                        class: "mt-2 text-sm text-green-700",
                        "Imported module: {name.read()}"
                    }
                },
                None => rsx! {},
            }
            div {
                class: "mt-4 flex justify-end",
                Button {
                    variant: ButtonVariant::Outline,
                    onclick: move |_| on_close.call(()),
                    "Close"
                }
            }
        }
    }
}

#[component]
fn FolderContentSection(title: &'static str, icon: &'static str, names: Vec<String>) -> Element {
    if names.is_empty() {
        return rsx! {};
    }

    rsx! {
        div {
            class: "space-y-1",
            div {
                class: "text-xs font-semibold uppercase tracking-wide text-gray-500",
                "{title}"
            }
            div {
                class: "space-y-1",
                for (index, name) in names.iter().enumerate() {
                    div {
                        key: "{title}-{index}",
                        class: "text-sm text-gray-700",
                        "{icon} {name}"
                    }
                }
            }
        }
    }
}

fn sorted_note_names(folder: &Folder) -> Vec<String> {
    let mut names: Vec<String> = folder.notes.keys().cloned().collect();
    names.sort();
    names
}

fn sorted_scene_names(game: &Game, folder: &Folder) -> Vec<String> {
    let mut names: Vec<String> = folder
        .scenes
        .iter()
        .filter_map(|id| game.scenes.get(id).map(|scene| scene.name.clone()))
        .collect();
    names.sort();
    names
}

fn sorted_creature_names(game: &Game, folder: &Folder) -> Vec<String> {
    let mut names: Vec<String> = folder
        .creatures
        .iter()
        .filter_map(|id| game.creatures.get(id).map(|creature| creature.name.clone()))
        .collect();
    names.sort();
    names
}

fn sorted_item_names(game: &Game, folder: &Folder) -> Vec<String> {
    let mut names: Vec<String> = folder
        .items
        .iter()
        .filter_map(|id| game.items.get(id).map(|item| item.name.clone()))
        .collect();
    names.sort();
    names
}

fn sorted_ability_names(game: &Game, folder: &Folder) -> Vec<String> {
    let mut names: Vec<String> = folder
        .abilities
        .iter()
        .filter_map(|id| game.abilities.get(id).map(|ability| ability.name.clone()))
        .collect();
    names.sort();
    names
}

fn sorted_class_names(game: &Game, folder: &Folder) -> Vec<String> {
    let mut names: Vec<String> = folder
        .classes
        .iter()
        .filter_map(|id| game.classes.get(id).map(|class| class.name.clone()))
        .collect();
    names.sort();
    names
}

fn parse_module_file_contents(contents: &str) -> anyhow::Result<Game> {
    if let Ok(game) = serde_json::from_str::<Game>(contents) {
        return Ok(game);
    }

    if let Ok(game) = serde_json::from_str::<SerializedGame>(contents) {
        return Ok(Game::from_serialized_game(game));
    }

    #[derive(serde::Deserialize)]
    struct WrappedGame {
        game: Game,
    }
    if let Ok(wrapped) = serde_json::from_str::<WrappedGame>(contents) {
        return Ok(wrapped.game);
    }

    #[derive(serde::Deserialize)]
    struct WrappedSerializedGame {
        game: SerializedGame,
    }
    if let Ok(wrapped) = serde_json::from_str::<WrappedSerializedGame>(contents) {
        return Ok(Game::from_serialized_game(wrapped.game));
    }

    Err(anyhow::anyhow!(
        "File does not look like a valid Arpeggio module JSON."
    ))
}

fn folder_path_label(path: &FolderPath) -> String {
    if path.is_root() {
        "/".to_string()
    } else {
        path.to_string()
    }
}

#[component]
fn Invitations(game_id: GameID) -> Element {
    let ws = use_ws();
    let mut invitations: Signal<Option<Vec<InvitationID>>> = use_signal(|| None);
    let mut load_error: Signal<Option<String>> = use_signal(|| None);

    // Load invitations on mount
    let _loader: Resource<()> = use_resource(move || async move {
        match send_request::<Vec<InvitationID>>(RPIGameRequest::GMListInvitations, ws).await {
            Ok(list) => {
                invitations.set(Some(list));
            }
            Err(e) => {
                error!(?e, "Failed to load invitations");
                load_error.set(Some(format!("{e}")));
            }
        }
    });

    let mut generate_action = use_action(move |_: ()| async move {
        let new_id = send_request::<InvitationID>(RPIGameRequest::GMGenerateInvitation, ws).await?;
        info!(?new_id, "Generated new invitation");
        let mut current = invitations().unwrap_or_default();
        current.push(new_id);
        invitations.set(Some(current));
        Ok::<(), anyhow::Error>(())
    });

    let mut delete_action = use_action(move |invitation_id: InvitationID| async move {
        send_request::<serde_json::Value>(RPIGameRequest::GMDeleteInvitation { invitation_id }, ws)
            .await?;
        info!(?invitation_id, "Deleted invitation");
        let current = invitations()
            .unwrap_or_default()
            .into_iter()
            .filter(|id| *id != invitation_id)
            .collect();
        invitations.set(Some(current));
        Ok::<(), anyhow::Error>(())
    });

    let base_url = {
        let window = web_sys::window().expect("window must exist");
        let location = window.location();
        let origin = location.origin().unwrap_or_default();
        origin
    };

    rsx! {
        div {
            class: "bg-white rounded-lg shadow-md p-4",
            div {
                class: "flex items-center justify-between mb-3",
                h2 {
                    class: "text-lg font-semibold text-gray-800",
                    "Invitations"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    disabled: generate_action.pending(),
                    onclick: move |_| generate_action.call(()),
                    if generate_action.pending() {
                        "Generating..."
                    } else {
                        "Generate New Link"
                    }
                }
            }

            if let Some(err) = load_error() {
                p {
                    class: "text-red-600 text-sm",
                    "Failed to load invitations: {err}"
                }
            }

            match invitations() {
                None => rsx! {
                    p {
                        class: "text-sm text-gray-500",
                        "Loading invitations..."
                    }
                },
                Some(list) if list.is_empty() => rsx! {
                    p {
                        class: "text-sm text-gray-500 italic",
                        "No invitation links yet. Generate one to invite players."
                    }
                },
                Some(list) => rsx! {
                    ul {
                        class: "space-y-2",
                        for invitation_id in list {
                            li {
                                key: "{invitation_id}",
                                class: "flex items-center gap-2",
                                input {
                                    class: "border rounded px-3 py-1 text-sm flex-1 bg-gray-50 text-gray-700",
                                    r#type: "text",
                                    readonly: true,
                                    value: "{base_url}/invitations/{game_id}/{invitation_id}",
                                }
                                Button {
                                    variant: ButtonVariant::Outline,
                                    disabled: delete_action.pending(),
                                    onclick: move |_| delete_action.call(invitation_id),
                                    "Delete"
                                }
                            }
                        }
                    }
                },
            }
        }
    }
}
