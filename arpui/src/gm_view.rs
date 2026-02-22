use arptypes::{
    Folder, FolderItemID, GMCommand, Game, GameLog, ModuleSource, SceneID, SerializedGame,
    multitenant::{GameAndMetadata, GameID, InvitationID, RPIGameRequest, Role},
};
use dioxus::prelude::*;
use foldertree::FolderPath;
use std::{collections::HashSet, rc::Rc};
use tracing::{error, info};

use crate::{
    GAME_LOGS, GAME_NAME, GAME_SOURCE, GameSource, Route,
    chat::GMChat,
    components::{
        button::{Button, ButtonVariant},
        modal::Modal,
        split_pane::{SplitDirection, SplitPane},
        tabs::{TabContent, TabList, TabTrigger, Tabs},
    },
    gfx::dioxus::Scene3dView,
    grid::{CreatureMenuAction, SceneGrid},
    rpi::{Connector, send_request, use_ws},
};

#[derive(Clone, Copy)]
struct GMGameContext(Memo<Game>);

fn use_gm_game() -> Game {
    let game = use_context::<GMGameContext>().0;
    game()
}

#[derive(Clone, Copy, PartialEq)]
enum GMSceneViewMode {
    TwoD,
    ThreeD,
}

#[component]
pub fn GMGamePage(id: GameID, #[props(default)] scene_path: Option<Vec<String>>) -> Element {
    rsx! {
        Connector {
            role: Role::GM,
            game_id: id,
            player_id: None,

            GameLoader {
                game_id: id,
                initial_scene_path: scene_path,
            }
        }
    }
}

#[component]
pub fn GMGameScenePage(id: GameID, scene_path: Vec<String>) -> Element {
    rsx! {
        GMGamePage {
            id,
            scene_path: Some(scene_path),
        }
    }
}

#[component]
fn GameLoader(game_id: GameID, initial_scene_path: Option<Vec<String>>) -> Element {
    let ws = use_ws();
    let future: Resource<anyhow::Result<Game>> = use_resource(move || async move {
        info!("fetching game state for GM view");
        let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
        let game = Game::from_serialized_game(response.game);
        *GAME_SOURCE.write() = GameSource::GM(game.clone());
        *GAME_LOGS.write() = response.logs;
        *GAME_NAME.write() = response.metadata.name.clone();
        Ok(game)
    });

    match &*future.read() {
        Some(Ok(_game)) => {
            rsx! {
                GMGameProvider {
                    Shell {
                        game_id,
                        initial_scene_path,
                    }
                }
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
fn GMGameProvider(children: Element) -> Element {
    let game = use_memo(move || match GAME_SOURCE() {
        GameSource::GM(game) => game,
        GameSource::Player { .. } => {
            panic!("GM game context used while current game source is Player")
        }
    });
    use_context_provider(move || GMGameContext(game));
    children
}

#[component]
fn Shell(game_id: GameID, initial_scene_path: Option<Vec<String>>) -> Element {
    let game = use_gm_game();
    let navigator = navigator();
    let initial_scene_id = initial_scene_path
        .as_ref()
        .and_then(|path| resolve_scene_id_from_route_path(&game, path));
    let mut selected_scene_id = use_signal(|| initial_scene_id.or(game.active_scene));
    let mut scene_view_mode = use_signal(|| GMSceneViewMode::ThreeD);
    let shown_scene_id = selected_scene_id().or(game.active_scene);
    let shown_scene = shown_scene_id.and_then(|sid| game.scenes.get(&sid).cloned());
    let gm_creature_actions: Option<Callback<arptypes::CreatureID, Vec<CreatureMenuAction>>> = None;
    let gm_creature_actions_3d = move |_creature_id| {
        vec![CreatureMenuAction::GMWalk, CreatureMenuAction::Teleport]
    };
    let navigate_to_scene = {
        let game = game.clone();
        let game_id = game_id.clone();
        move |scene_id: SceneID| {
            if let Some(scene_path) = route_scene_path_for_scene_id(&game, scene_id) {
                navigator.push(Route::GMGameScenePage {
                    id: game_id.clone(),
                    scene_path,
                });
            } else {
                navigator.push(Route::GMGamePage {
                    id: game_id.clone(),
                });
            }
        }
    };

    rsx! {
        div {
            class: "flex h-full min-h-0 w-full overflow-hidden",
            div {
                class: "grow min-h-0 min-w-0 relative",
                match scene_view_mode() {
                    GMSceneViewMode::TwoD => rsx! {
                        SceneGrid {
                            scene: shown_scene.clone(),
                            get_creature_actions: gm_creature_actions,
                        }
                    },
                    GMSceneViewMode::ThreeD => rsx! {
                        if let Some(scene) = shown_scene.clone() {
                            Scene3dView {
                                key: "{scene.id}",
                                scene: scene,
                                get_creature_actions: gm_creature_actions_3d,
                            }
                        } else {
                            div {
                                class: "w-full h-full flex items-center justify-center text-gray-500",
                                "Select a scene."
                            }
                        }
                    }
                }
                div {
                    class: "absolute top-3 left-3 z-20 inline-flex items-center gap-1 rounded-md border border-gray-300 bg-white/90 p-1 shadow-sm backdrop-blur-sm",
                    button {
                        r#type: "button",
                        class: if scene_view_mode() == GMSceneViewMode::TwoD {
                            "px-3 py-1 text-sm rounded bg-blue-600 text-white"
                        } else {
                            "px-3 py-1 text-sm rounded text-gray-700 hover:bg-gray-100"
                        },
                        onclick: move |_| scene_view_mode.set(GMSceneViewMode::TwoD),
                        "2D"
                    }
                    button {
                        r#type: "button",
                        class: if scene_view_mode() == GMSceneViewMode::ThreeD {
                            "px-3 py-1 text-sm rounded bg-blue-600 text-white"
                        } else {
                            "px-3 py-1 text-sm rounded text-gray-700 hover:bg-gray-100"
                        },
                        onclick: move |_| scene_view_mode.set(GMSceneViewMode::ThreeD),
                        "3D"
                    }
                }
            }
            div {
                class: "w-[30rem] h-full min-h-0 overflow-hidden border-l border-gray-200 bg-white flex flex-col",
                style: "min-height: min(800px, 100%);",
                SplitPane {
                    direction: SplitDirection::Vertical,
                    initial_size: 70.0,
                    min_size: 35.0,
                    max_size: 90.0,
                    first: rsx! {
                        Tabs {
                            class: "h-full min-h-0 flex flex-col overflow-hidden".to_string(),
                            default_value: "campaign".to_string(),
                            TabList {
                                TabTrigger { value: "campaign".to_string(), index: 0usize, "Campaign" }
                                TabTrigger { value: "players".to_string(), index: 1usize, "Players" }
                                TabTrigger { value: "invitations".to_string(), index: 2usize, "Invitations" }
                            }
                            TabContent {
                                class: "h-full min-h-0 overflow-hidden".to_string(),
                                index: 0usize,
                                value: "campaign".to_string(),
                                div {
                                    class: "h-full min-h-0 overflow-y-auto p-4",
                                    CampaignTreeCard {
                                        selected_scene_id: shown_scene_id,
                                        on_select_scene: move |scene_id| {
                                            selected_scene_id.set(Some(scene_id));
                                            navigate_to_scene(scene_id);
                                        },
                                    }
                                }
                            }
                            TabContent {
                                class: "h-full min-h-0 overflow-hidden".to_string(),
                                index: 1usize,
                                value: "players".to_string(),
                                div {
                                    class: "h-full min-h-0 overflow-y-auto p-4",
                                    PlayersTab {
                                        current_scene_id: shown_scene_id,
                                    }
                                }
                            }
                            TabContent {
                                class: "h-full min-h-0 overflow-hidden".to_string(),
                                index: 2usize,
                                value: "invitations".to_string(),
                                div {
                                    class: "h-full min-h-0 overflow-y-auto p-4",
                                    Invitations { game_id }
                                }
                            }
                        }
                    },
                    second: rsx! {
                        div {
                            class: "h-full min-h-0 p-4",
                            GMChat {}
                        }
                    },
                }
            }
        }
    }
}

#[component]
fn CampaignTreeCard(
    selected_scene_id: Option<SceneID>,
    on_select_scene: EventHandler<SceneID>,
) -> Element {
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
                selected_scene_id,
                on_select_scene,
            }
        }
    }
}

#[component]
fn CampaignFolder(
    path: FolderPath,
    display_name: String,
    depth: usize,
    start_open: bool,
    selected_scene_id: Option<SceneID>,
    on_select_scene: EventHandler<SceneID>,
) -> Element {
    let game = use_gm_game();
    let mut is_expanded = use_signal(move || start_open);
    let mut show_menu = use_signal(|| false);
    let mut show_import_modal = use_signal(|| false);
    let mut show_move_modal = use_signal(|| false);
    let mut show_rename_modal = use_signal(|| false);
    let mut show_delete_modal = use_signal(|| false);

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
    let scene_entries = sorted_scene_entries(&game, folder);
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
                        if !path.is_root() {
                            button {
                                class: "block w-full px-3 py-2 text-left text-sm text-gray-700 hover:bg-gray-100",
                                onclick: move |evt: Event<MouseData>| {
                                    evt.stop_propagation();
                                    show_menu.set(false);
                                    show_rename_modal.set(true);
                                },
                                "Rename this folder"
                            }
                            button {
                                class: "block w-full px-3 py-2 text-left text-sm text-gray-700 hover:bg-gray-100",
                                onclick: move |evt: Event<MouseData>| {
                                    evt.stop_propagation();
                                    show_menu.set(false);
                                    show_move_modal.set(true);
                                },
                                "Move this folder"
                            }
                            button {
                                class: "block w-full px-3 py-2 text-left text-sm text-red-700 hover:bg-red-50",
                                onclick: move |evt: Event<MouseData>| {
                                    evt.stop_propagation();
                                    show_menu.set(false);
                                    show_delete_modal.set(true);
                                },
                                "Delete this folder"
                            }
                            div {
                                class: "my-1 border-t border-gray-200"
                            }
                        }
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

            if show_move_modal() {
                MoveFolderModal {
                    path: path.clone(),
                    on_close: move |_| show_move_modal.set(false),
                }
            }

            if show_rename_modal() {
                RenameFolderModal {
                    path: path.clone(),
                    on_close: move |_| show_rename_modal.set(false),
                }
            }

            if show_delete_modal() {
                DeleteFolderModal {
                    path: path.clone(),
                    on_close: move |_| show_delete_modal.set(false),
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
                    SceneContentSection {
                        scenes: scene_entries,
                        selected_scene_id,
                        on_select_scene: on_select_scene.clone(),
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
                                selected_scene_id,
                                on_select_scene: on_select_scene.clone(),
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
fn RenameFolderModal(path: FolderPath, on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let Some((parent_path, current_name)) = path.up() else {
        return rsx! {};
    };

    let mut new_name = use_signal(|| current_name.clone());
    let mut rename_action = use_action({
        let ws = ws.clone();
        let parent_path = parent_path.clone();
        let current_name = current_name.clone();
        let on_close = on_close.clone();
        move |next_name: String| {
            let ws = ws.clone();
            let parent_path = parent_path.clone();
            let current_name = current_name.clone();
            let on_close = on_close.clone();
            async move {
                let command = GMCommand::RenameFolderItem {
                    path: parent_path,
                    item_id: FolderItemID::SubfolderID(current_name),
                    new_name: next_name,
                };
                let result = send_request::<Result<Vec<GameLog>, String>>(
                    RPIGameRequest::GMCommand {
                        command: Box::new(command),
                    },
                    ws,
                )
                .await?;
                result.map_err(anyhow::Error::msg)?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });

    let can_rename = {
        let candidate = new_name().trim().to_string();
        !candidate.is_empty() && candidate != current_name
    };

    rsx! {
        crate::components::modal::Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            h3 {
                class: "text-lg font-semibold text-gray-800 mb-2",
                "Rename Folder"
            }
            p {
                class: "text-sm text-gray-600 mb-4",
                "Current name: {current_name}"
            }
            input {
                class: "w-full rounded border border-gray-300 px-3 py-2 text-sm",
                r#type: "text",
                value: "{new_name}",
                autofocus: true,
                oninput: move |evt| new_name.set(evt.value()),
                onkeydown: move |evt| {
                    if evt.key() == Key::Enter && can_rename && !rename_action.pending() {
                        let next_name = new_name().trim().to_string();
                        rename_action.call(next_name);
                    }
                },
            }
            match rename_action.value() {
                Some(Err(err)) => rsx! {
                    p {
                        class: "mt-2 text-sm text-red-600",
                        "Rename failed: {err}"
                    }
                },
                _ => rsx! {},
            }
            div {
                class: "mt-4 flex justify-end gap-2",
                Button {
                    variant: ButtonVariant::Outline,
                    onclick: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    disabled: !can_rename || rename_action.pending(),
                    onclick: move |_| {
                        let next_name = new_name().trim().to_string();
                        rename_action.call(next_name);
                    },
                    if rename_action.pending() {
                        "Renaming..."
                    } else {
                        "Rename"
                    }
                }
            }
        }
    }
}

#[component]
fn DeleteFolderModal(path: FolderPath, on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let Some((parent_path, current_name)) = path.up() else {
        return rsx! {};
    };

    let folder_label = folder_path_label(&path);
    let mut delete_action = use_action({
        let ws = ws.clone();
        let parent_path = parent_path.clone();
        let current_name = current_name.clone();
        let on_close = on_close.clone();
        move || {
            let ws = ws.clone();
            let parent_path = parent_path.clone();
            let current_name = current_name.clone();
            let on_close = on_close.clone();
            async move {
                let command = GMCommand::DeleteFolderItem {
                    path: parent_path,
                    item_id: FolderItemID::SubfolderID(current_name),
                };
                let result = send_request::<Result<Vec<GameLog>, String>>(
                    RPIGameRequest::GMCommand {
                        command: Box::new(command),
                    },
                    ws,
                )
                .await?;
                result.map_err(anyhow::Error::msg)?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
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
                "Delete Folder"
            }
            p {
                class: "text-sm text-gray-600 mb-4",
                "Delete {folder_label}? This only works if the folder is empty."
            }
            match delete_action.value() {
                Some(Err(err)) => rsx! {
                    p {
                        class: "mb-3 text-sm text-red-600",
                        "Delete failed: {err}"
                    }
                },
                _ => rsx! {},
            }
            div {
                class: "mt-4 flex justify-end gap-2",
                Button {
                    variant: ButtonVariant::Outline,
                    onclick: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Destructive,
                    disabled: delete_action.pending(),
                    onclick: move |_| delete_action.call(),
                    if delete_action.pending() {
                        "Deleting..."
                    } else {
                        "Delete"
                    }
                }
            }
        }
    }
}

#[component]
fn MoveFolderModal(path: FolderPath, on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let game = use_gm_game();
    let mut search_term = use_signal(String::new);
    let mut selected_destination = use_signal(|| None::<FolderPath>);

    let source_path_parts = path.up();
    let folder_name = folder_path_label(&path);

    let mut move_action = use_action({
        let ws = ws.clone();
        let on_close = on_close.clone();
        let source_path_parts = source_path_parts.clone();
        move |destination: FolderPath| {
            let ws = ws.clone();
            let on_close = on_close.clone();
            let source_path_parts = source_path_parts.clone();
            async move {
                let (source, folder_name) = source_path_parts
                    .clone()
                    .ok_or_else(|| anyhow::anyhow!("Root folder cannot be moved."))?;
                let item_id = FolderItemID::SubfolderID(folder_name);
                let command = GMCommand::MoveFolderItem {
                    source,
                    item_id,
                    destination,
                };
                let result = send_request::<Result<Vec<GameLog>, String>>(
                    RPIGameRequest::GMCommand {
                        command: Box::new(command),
                    },
                    ws,
                )
                .await?;
                result.map_err(anyhow::Error::msg)?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });

    let mut all_folders = collect_all_folder_paths(&game);
    all_folders.sort();

    let search_lower = search_term().to_lowercase();
    let mut filtered_destinations: Vec<FolderPath> = all_folders
        .into_iter()
        .filter(|destination| {
            let label = folder_path_label(destination).to_lowercase();
            label.contains(&search_lower)
        })
        .collect();
    if filtered_destinations.len() > 60 {
        filtered_destinations.truncate(60);
    }

    rsx! {
        crate::components::modal::Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            h3 {
                class: "text-lg font-semibold text-gray-800 mb-2",
                "Move Folder"
            }
            p {
                class: "text-sm text-gray-600 mb-4",
                "Move {folder_name} to a destination folder."
            }

            input {
                class: "w-full rounded border border-gray-300 px-3 py-2 text-sm",
                r#type: "text",
                placeholder: "Search folders...",
                value: "{search_term}",
                oninput: move |evt| search_term.set(evt.value()),
            }

            div {
                class: "mt-3 max-h-72 overflow-y-auto rounded border border-gray-200",
                if filtered_destinations.is_empty() {
                    div {
                        class: "px-3 py-2 text-sm text-gray-500",
                        "No folders match your search."
                    }
                } else {
                    for destination in filtered_destinations {
                        {
                            let label = folder_path_label(&destination);
                            let is_selected = selected_destination()
                                .as_ref()
                                .is_some_and(|selected| selected == &destination);
                            rsx! {
                                button {
                                    key: "{label}",
                                    class: if is_selected {
                                        "block w-full border-b border-gray-100 bg-blue-50 px-3 py-2 text-left text-sm text-blue-800"
                                    } else {
                                        "block w-full border-b border-gray-100 px-3 py-2 text-left text-sm text-gray-700 hover:bg-gray-50"
                                    },
                                    onclick: move |_| selected_destination.set(Some(destination.clone())),
                                    "{label}"
                                }
                            }
                        }
                    }
                }
            }

            match move_action.value() {
                Some(Err(err)) => rsx! {
                    p {
                        class: "mt-2 text-sm text-red-600",
                        "Move failed: {err}"
                    }
                },
                _ => rsx! {},
            }

            div {
                class: "mt-4 flex justify-end gap-2",
                Button {
                    variant: ButtonVariant::Outline,
                    onclick: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    disabled: selected_destination().is_none() || move_action.pending(),
                    onclick: move |_| {
                        if let Some(destination) = selected_destination() {
                            move_action.call(destination);
                        }
                    },
                    if move_action.pending() {
                        "Moving..."
                    } else {
                        "Move"
                    }
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
struct SceneEntry {
    id: SceneID,
    name: String,
}

#[component]
fn SceneContentSection(
    scenes: Vec<SceneEntry>,
    selected_scene_id: Option<SceneID>,
    on_select_scene: EventHandler<SceneID>,
) -> Element {
    if scenes.is_empty() {
        return rsx! {};
    }

    rsx! {
        div {
            class: "space-y-1",
            div {
                class: "text-xs font-semibold uppercase tracking-wide text-gray-500",
                "Scenes"
            }
            div {
                class: "space-y-1",
                for scene in scenes {
                    {
                        let is_selected = selected_scene_id == Some(scene.id);
                        rsx! {
                            button {
                                key: "{scene.id}",
                                class: if is_selected {
                                    "block w-full rounded bg-blue-50 px-2 py-1 text-left text-sm text-blue-800"
                                } else {
                                    "block w-full rounded px-2 py-1 text-left text-sm text-gray-700 hover:bg-gray-50"
                                },
                                onclick: move |_| on_select_scene.call(scene.id),
                                "[S] {scene.name}"
                            }
                        }
                    }
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

fn sorted_scene_entries(game: &Game, folder: &Folder) -> Vec<SceneEntry> {
    let mut entries: Vec<SceneEntry> = folder
        .scenes
        .iter()
        .filter_map(|id| {
            game.scenes.get(id).map(|scene| SceneEntry {
                id: scene.id,
                name: scene.name.clone(),
            })
        })
        .collect();
    entries.sort_by(|a, b| a.name.cmp(&b.name));
    entries
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

fn collect_all_folder_paths(game: &Game) -> Vec<FolderPath> {
    game.campaign
        .walk_paths(&FolderPath::root())
        .cloned()
        .collect()
}

fn resolve_scene_id_from_route_path(game: &Game, scene_path: &[String]) -> Option<SceneID> {
    if scene_path.is_empty() {
        return None;
    }

    let folder_segments = scene_path[..scene_path.len() - 1].to_vec();
    let scene_name = &scene_path[scene_path.len() - 1];
    let folder_path = FolderPath::from_vec(folder_segments);
    let folder = game.campaign.get(&folder_path).ok()?;

    folder.scenes.iter().find_map(|scene_id| {
        game.scenes
            .get(scene_id)
            .filter(|scene| &scene.name == scene_name)
            .map(|scene| scene.id)
    })
}

fn route_scene_path_for_scene_id(game: &Game, scene_id: SceneID) -> Option<Vec<String>> {
    let scene_name = game.scenes.get(&scene_id)?.name.clone();
    let root = FolderPath::root();

    for folder_path in game.campaign.walk_paths(&root) {
        let Ok(folder) = game.campaign.get(folder_path) else {
            continue;
        };
        if folder.scenes.contains(&scene_id) {
            let mut path: Vec<String> = folder_path.clone().into_vec();
            path.push(scene_name);
            return Some(path);
        }
    }

    None
}

#[derive(Clone, Default, PartialEq)]
struct CreatureSelectorSearchState {
    query: String,
    matching_creatures: HashSet<arptypes::CreatureID>,
    expanded_paths: HashSet<FolderPath>,
}

#[derive(Clone, PartialEq)]
struct CreatureFolderEntry {
    id: arptypes::CreatureID,
    name: String,
}

fn sorted_creature_entries(game: &Game, folder: &Folder) -> Vec<CreatureFolderEntry> {
    let mut entries: Vec<CreatureFolderEntry> = folder
        .creatures
        .iter()
        .filter_map(|id| {
            game.creatures.get(id).map(|creature| CreatureFolderEntry {
                id: creature.id,
                name: creature.name.clone(),
            })
        })
        .collect();
    entries.sort_by(|a, b| {
        a.name
            .cmp(&b.name)
            .then(a.id.to_string().cmp(&b.id.to_string()))
    });
    entries
}

fn add_path_and_ancestors(path: FolderPath, paths: &mut HashSet<FolderPath>) {
    paths.insert(path.clone());
    let mut cursor = path;
    while let Some((parent, _)) = cursor.up() {
        paths.insert(parent.clone());
        cursor = parent;
    }
}

fn build_creature_selector_search_state(game: &Game, query: &str) -> CreatureSelectorSearchState {
    let normalized = query.trim().to_lowercase();
    if normalized.is_empty() {
        return CreatureSelectorSearchState::default();
    }

    let mut state = CreatureSelectorSearchState {
        query: normalized.clone(),
        ..Default::default()
    };

    for path in game.campaign.walk_paths(&FolderPath::root()) {
        let Ok(folder) = game.campaign.get(path) else {
            continue;
        };
        for creature_id in &folder.creatures {
            let Some(creature) = game.creatures.get(creature_id) else {
                continue;
            };
            if creature.name.to_lowercase().contains(&normalized) {
                state.matching_creatures.insert(*creature_id);
                add_path_and_ancestors(path.clone(), &mut state.expanded_paths);
            }
        }
    }

    state
}

// TODO: Generalize this folder selector so it can render/select other campaign object types
// (e.g. items) instead of being creature-specific.
#[component]
fn CreatureSelectorFolder(
    path: FolderPath,
    display_name: String,
    depth: usize,
    start_open: bool,
    selected: Signal<HashSet<arptypes::CreatureID>>,
    search_state: Rc<CreatureSelectorSearchState>,
) -> Element {
    let game = use_gm_game();
    let mut is_expanded = use_signal(move || start_open);

    if !search_state.query.is_empty() && !search_state.expanded_paths.contains(&path) {
        return rsx! {};
    }

    let folder = match game.campaign.get(&path) {
        Ok(folder) => folder,
        Err(_) => {
            return rsx! {};
        }
    };

    let mut child_names: Vec<String> = match game.campaign.get_children(&path) {
        Ok(children) => children.iter().cloned().collect(),
        Err(_) => Vec::new(),
    };
    child_names.sort();

    let mut creature_entries = sorted_creature_entries(&game, folder);
    if !search_state.query.is_empty() {
        creature_entries.retain(|entry| search_state.matching_creatures.contains(&entry.id));
        child_names.retain(|child| {
            search_state
                .expanded_paths
                .contains(&path.child(child.clone()))
        });
    }

    let force_open = !search_state.query.is_empty() && search_state.expanded_paths.contains(&path);
    let expanded = force_open || is_expanded();
    let path_key = if path.is_root() {
        "/".to_string()
    } else {
        path.to_string()
    };
    let padding_style = format!("margin-left: {}rem;", depth as f32 * 0.75);

    rsx! {
        div {
            class: "space-y-1",
            style: "{padding_style}",
            button {
                class: "flex w-full items-center gap-2 rounded px-2 py-1 text-left text-sm text-gray-800 hover:bg-gray-100",
                onclick: move |_| {
                    if search_state.query.is_empty() {
                        is_expanded.toggle();
                    }
                },
                span {
                    class: "text-xs text-gray-500",
                    if expanded { "v" } else { ">" }
                }
                span {
                    class: "font-medium",
                    "Folder: {display_name}"
                }
            }

            if expanded {
                div {
                    class: "ml-3 space-y-1 border-l border-gray-200 pl-3",
                    if !creature_entries.is_empty() {
                        div {
                            class: "pt-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                            "Creatures"
                        }
                    }
                    for creature in creature_entries {
                        {
                            let is_selected = selected().contains(&creature.id);
                            let creature_id = creature.id;
                            rsx! {
                                label {
                                    key: "{path_key}/c/{creature_id}",
                                    class: "flex cursor-pointer items-center gap-2 rounded px-2 py-1 text-sm text-gray-700 hover:bg-gray-50",
                                    input {
                                        r#type: "checkbox",
                                        checked: is_selected,
                                        onchange: move |evt| {
                                            let checked = evt.checked();
                                            selected.with_mut(|set| {
                                                if checked {
                                                    set.insert(creature_id);
                                                } else {
                                                    set.remove(&creature_id);
                                                }
                                            });
                                        },
                                    }
                                    span { "{creature.name}" }
                                }
                            }
                        }
                    }

                    for child_name in child_names {
                        CreatureSelectorFolder {
                            key: "{path_key}/{child_name}",
                            path: path.child(child_name.clone()),
                            display_name: child_name,
                            depth: depth + 1,
                            start_open: false,
                            selected,
                            search_state: search_state.clone(),
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn PlayersTab(current_scene_id: Option<SceneID>) -> Element {
    let game = use_gm_game();
    let ws = use_ws();
    let mut grant_target = use_signal(|| None::<arptypes::PlayerID>);
    let current_scene_name = current_scene_id
        .and_then(|sid| game.scenes.get(&sid))
        .map(|s| s.name.clone());

    let mut set_scene_action = use_action(
        move |(player_id, scene_id): (arptypes::PlayerID, Option<SceneID>)| {
            let ws = ws;
            async move {
                let result = send_request::<Result<Vec<GameLog>, String>>(
                    RPIGameRequest::GMCommand {
                        command: Box::new(GMCommand::SetPlayerScene {
                            player_id,
                            scene_id,
                        }),
                    },
                    ws,
                )
                .await?;
                if let Err(msg) = result {
                    return Err(anyhow::anyhow!(msg));
                }
                Ok::<(), anyhow::Error>(())
            }
        },
    );

    let mut players: Vec<_> = game.players.iter().cloned().collect();
    players.sort_by(|a, b| a.player_id.to_string().cmp(&b.player_id.to_string()));

    rsx! {
        div {
            class: "bg-white rounded-lg shadow-md p-4",
            div {
                class: "mb-3",
                h2 {
                    class: "text-lg font-semibold text-gray-800",
                    "Players"
                }
                p {
                    class: "text-sm text-gray-600",
                    if let Some(scene_name) = current_scene_name.clone() {
                        "Current scene target: {scene_name}"
                    } else {
                        "Select a scene to enable \"Move to current scene\"."
                    }
                }
            }

            if let Some(Err(err)) = set_scene_action.value() {
                p {
                    class: "mb-3 text-sm text-red-600",
                    "Failed to update player scene: {err}"
                }
            }

            if players.is_empty() {
                p {
                    class: "text-sm text-gray-500 italic",
                    "No registered players yet."
                }
            } else {
                div {
                    class: "space-y-2",
                    for player in players {
                        {
                            let mut creature_names: Vec<String> = player
                                .creatures
                                .iter()
                                .map(|cid| {
                                    game.creatures
                                        .get(cid)
                                        .map(|c| c.name.clone())
                                        .unwrap_or_else(|| cid.to_string())
                                })
                                .collect();
                            creature_names.sort();

                            let scene_name = player
                                .scene
                                .and_then(|sid| game.scenes.get(&sid))
                                .map(|s| s.name.clone())
                                .unwrap_or_else(|| "No scene".to_string());

                            let player_id = player.player_id.clone();
                            let player_scene = player.scene;
                            let player_id_for_remove = player_id.clone();
                            let player_id_for_move = player_id.clone();

                            rsx! {
                                div {
                                    key: "{player_id}",
                                    class: "rounded-lg border border-gray-200 bg-white p-4 shadow-sm",
                                    div {
                                        class: "flex items-start justify-between gap-3",
                                        p {
                                            class: "min-w-0 text-lg font-semibold leading-tight text-gray-900",
                                            "{player_id}"
                                        }
                                        span {
                                            class: if player_scene.is_some() {
                                                "shrink-0 rounded-full bg-blue-50 px-2 py-0.5 text-xs font-medium text-blue-700"
                                            } else {
                                                "shrink-0 rounded-full bg-gray-100 px-2 py-0.5 text-xs font-medium text-gray-600"
                                            },
                                            "{scene_name}"
                                        }
                                    }

                                    div {
                                        class: "mt-3",
                                        p {
                                            class: "mb-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                                            "Creatures"
                                        }
                                        if creature_names.is_empty() {
                                            p {
                                                class: "text-sm text-gray-500 italic",
                                                "None"
                                            }
                                        } else {
                                            div {
                                                class: "flex flex-wrap gap-1.5",
                                                for (idx, creature_name) in creature_names.iter().enumerate() {
                                                    span {
                                                        key: "{player_id}-creature-{idx}",
                                                        class: "rounded-md border border-gray-200 bg-gray-50 px-2 py-1 text-xs text-gray-700",
                                                        "{creature_name}"
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    div {
                                        class: "mt-4 flex flex-wrap items-center gap-2",
                                        Button {
                                            variant: ButtonVariant::Outline,
                                            onclick: move |_| grant_target.set(Some(player_id.clone())),
                                            "Manage creatures"
                                        }
                                        if player_scene.is_some() {
                                            Button {
                                                variant: ButtonVariant::Outline,
                                                disabled: set_scene_action.pending(),
                                                onclick: move |_| set_scene_action.call((player_id_for_remove.clone(), None)),
                                                "Remove from scene"
                                            }
                                        }

                                        if let Some(target_scene_id) = current_scene_id {
                                            if player_scene != Some(target_scene_id) {
                                                Button {
                                                    variant: ButtonVariant::Primary,
                                                    disabled: set_scene_action.pending(),
                                                    onclick: move |_| {
                                                        set_scene_action.call((player_id_for_move.clone(), Some(target_scene_id)))
                                                    },
                                                    "Move to current scene"
                                                }
                                            } else {
                                                span {
                                                    class: "rounded-full bg-green-50 px-2 py-1 text-xs font-medium text-green-700",
                                                    "In current scene"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if let Some(player_id) = grant_target() {
                GrantCreaturesModal {
                    player_id,
                    on_close: move |_| grant_target.set(None),
                }
            }
        }
    }
}

#[component]
fn GrantCreaturesModal(player_id: arptypes::PlayerID, on_close: EventHandler<()>) -> Element {
    let game = use_gm_game();
    let ws = use_ws();
    let mut search = use_signal(|| String::new());
    let initial_selected = game
        .players
        .get(&player_id)
        .map(|p| p.creatures.clone())
        .unwrap_or_default();
    let selected = use_signal(move || initial_selected);

    let mut manage_action = use_action({
        let player_id = player_id.clone();
        let on_close = on_close.clone();
        move |(to_grant, to_remove): (Vec<arptypes::CreatureID>, Vec<arptypes::CreatureID>)| {
            let player_id = player_id.clone();
            let on_close = on_close.clone();
            async move {
                if to_grant.is_empty() && to_remove.is_empty() {
                    return Ok::<(), anyhow::Error>(());
                }

                if !to_grant.is_empty() {
                    let result = send_request::<Result<Vec<GameLog>, String>>(
                        RPIGameRequest::GMCommand {
                            command: Box::new(GMCommand::GiveCreaturesToPlayer {
                                player_id: player_id.clone(),
                                creature_ids: to_grant,
                            }),
                        },
                        ws,
                    )
                    .await?;
                    if let Err(msg) = result {
                        return Err(anyhow::anyhow!(msg));
                    }
                }

                if !to_remove.is_empty() {
                    let result = send_request::<Result<Vec<GameLog>, String>>(
                        RPIGameRequest::GMCommand {
                            command: Box::new(GMCommand::RemoveCreaturesFromPlayer {
                                player_id,
                                creature_ids: to_remove,
                            }),
                        },
                        ws,
                    )
                    .await?;
                    if let Err(msg) = result {
                        return Err(anyhow::anyhow!(msg));
                    }
                }
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });

    let Some(player) = game.players.get(&player_id) else {
        return rsx! {
            Modal {
                open: true,
                on_close: move |_| on_close.call(()),
                class: "p-6",
                h3 {
                    class: "text-lg font-semibold text-gray-800 mb-2",
                    "Grant Creatures"
                }
                p {
                    class: "text-sm text-red-600",
                    "Player not found: {player_id}"
                }
                div {
                    class: "mt-4 flex justify-end",
                    Button {
                        variant: ButtonVariant::Ghost,
                        onclick: move |_| on_close.call(()),
                        "Close"
                    }
                }
            }
        };
    };

    let search_state = Rc::new(build_creature_selector_search_state(&game, &search()));
    let already_owned = player.creatures.clone();

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6 max-w-xl",
            h3 {
                class: "text-lg font-semibold text-gray-800 mb-2",
                "Manage Creatures"
            }
            p {
                class: "text-sm text-gray-600 mb-3",
                "Player: {player_id}"
            }

            input {
                class: "mb-3 w-full rounded border border-gray-300 px-3 py-2 text-sm",
                r#type: "text",
                placeholder: "Search creatures...",
                value: "{search}",
                oninput: move |evt| search.set(evt.value()),
            }

            div {
                class: "max-h-72 overflow-y-auto rounded border border-gray-200",
                if !search_state.query.is_empty() && search_state.matching_creatures.is_empty() {
                    p {
                        class: "p-3 text-sm text-gray-500",
                        "No creatures match your search."
                    }
                } else {
                    CreatureSelectorFolder {
                        path: FolderPath::root(),
                        display_name: "Campaign".to_string(),
                        depth: 0,
                        start_open: true,
                        selected,
                        search_state: search_state.clone(),
                    }
                }
            }

            if !already_owned.is_empty() {
                p {
                    class: "mt-2 text-xs text-gray-500",
                    "Creatures already granted to this player start checked."
                }
            }

            if let Some(Err(err)) = manage_action.value() {
                p {
                    class: "mt-2 text-sm text-red-600",
                    "Failed to update creatures: {err}"
                }
            }

            div {
                class: "mt-4 flex justify-end gap-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    onclick: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    disabled: manage_action.pending(),
                    onclick: move |_| {
                        let selected_now = selected();
                        let to_grant: Vec<_> = selected_now
                            .iter()
                            .filter(|cid| !already_owned.contains(cid))
                            .cloned()
                            .collect();
                        let to_remove: Vec<_> = already_owned
                            .iter()
                            .filter(|cid| !selected_now.contains(*cid))
                            .cloned()
                            .collect();

                        if to_grant.is_empty() && to_remove.is_empty() {
                            on_close.call(());
                        } else {
                            manage_action.call((to_grant, to_remove));
                        }
                    },
                    if manage_action.pending() {
                        "Saving..."
                    } else {
                        "Save"
                    }
                }
            }
        }
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
