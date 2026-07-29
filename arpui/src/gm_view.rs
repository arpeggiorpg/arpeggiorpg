use arptypes::{
    GMCommand, Game, GameLog, SceneID,
    multitenant::{GameAndMetadata, GameID, InvitationID, RPIGameRequest, Role},
};
use dioxus::prelude::*;
use std::collections::HashSet;
use tracing::{error, info};

use crate::{
    GAME_LOGS, GAME_NAME, GAME_SOURCE, GameSource, Route,
    catalog::CatalogPanel,
    chat::GMChat,
    components::{
        button::{Button, ButtonVariant},
        modal::Modal,
        split_pane::{SplitDirection, SplitPane},
        tabs::{TabContent, TabList, TabTrigger, Tabs},
    },
    gfx::dioxus::Scene3dView,
    grid::{CreatureMenuAction, SceneGrid},
    history::HistoryPanel,
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
    let gm_creature_actions_3d =
        move |_creature_id| vec![CreatureMenuAction::GMWalk, CreatureMenuAction::Teleport];
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
                            default_value: "catalog".to_string(),
                            TabList {
                                TabTrigger { value: "catalog".to_string(), index: 0usize, "Catalog" }
                                TabTrigger { value: "players".to_string(), index: 1usize, "Players" }
                                TabTrigger { value: "invitations".to_string(), index: 2usize, "Invitations" }
                                TabTrigger { value: "history".to_string(), index: 3usize, "History" }
                            }
                            TabContent {
                                class: "h-full min-h-0 overflow-hidden".to_string(),
                                index: 0usize,
                                value: "catalog".to_string(),
                                div {
                                    class: "h-full min-h-0 overflow-y-auto p-4",
                                    CatalogPanel {
                                        game: game.clone(),
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
                            TabContent {
                                class: "h-full min-h-0 overflow-hidden".to_string(),
                                index: 3usize,
                                value: "history".to_string(),
                                HistoryPanel {
                                    game: game.clone(),
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

fn resolve_scene_id_from_route_path(game: &Game, scene_path: &[String]) -> Option<SceneID> {
    let segment = scene_path.last()?;
    segment
        .parse()
        .ok()
        .filter(|id| game.scenes.contains_key(id))
}

fn route_scene_path_for_scene_id(game: &Game, scene_id: SceneID) -> Option<Vec<String>> {
    game.scenes
        .contains_key(&scene_id)
        .then(|| vec![scene_id.to_string()])
}

#[derive(Clone, PartialEq)]
struct CreatureSelectorEntry {
    id: arptypes::CreatureID,
    name: String,
}

fn filtered_creature_entries(
    game: &Game,
    query: &str,
    initially_selected: &HashSet<arptypes::CreatureID>,
) -> Vec<CreatureSelectorEntry> {
    let normalized_query = query.trim().to_lowercase();
    let mut entries: Vec<_> = game
        .creatures
        .values()
        .filter(|creature| {
            normalized_query.is_empty() || creature.name.to_lowercase().contains(&normalized_query)
        })
        .map(|creature| CreatureSelectorEntry {
            id: creature.id,
            name: creature.name.clone(),
        })
        .collect();
    entries.sort_by(|a, b| {
        initially_selected
            .contains(&b.id)
            .cmp(&initially_selected.contains(&a.id))
            .then_with(|| a.name.to_lowercase().cmp(&b.name.to_lowercase()))
            .then_with(|| a.id.to_string().cmp(&b.id.to_string()))
    });
    entries
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
    let mut selected = use_signal(move || initial_selected);

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

    let already_owned = player.creatures.clone();
    let creature_entries = filtered_creature_entries(&game, &search(), &already_owned);

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
                if creature_entries.is_empty() {
                    p {
                        class: "p-3 text-sm text-gray-500",
                        if search().trim().is_empty() {
                            "No creatures are available."
                        } else {
                            "No creatures match your search."
                        }
                    }
                } else {
                    div {
                        class: "divide-y divide-gray-100",
                        for creature in creature_entries {
                            {
                                let creature_id = creature.id;
                                let is_selected = selected().contains(&creature_id);
                                rsx! {
                                    label {
                                        key: "{creature_id}",
                                        class: "flex cursor-pointer items-center gap-3 px-3 py-2 text-sm text-gray-700 hover:bg-gray-50",
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
