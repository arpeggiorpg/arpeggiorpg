use arptypes::{
    AABB, AbilityCreation, Action, ClassCreation, CreatureCreation, CreatureEffect, CreatureTarget,
    Dice, Energy, Folder, GMCommand, Game, Note, SceneCreation, SceneID,
    multitenant::RPIGameRequest, u32meter,
};
use dioxus::prelude::*;
use foldertree::FolderPath;

use crate::{
    components::{
        button::{Button, ButtonVariant},
        modal::Modal,
    },
    rpi::{send_request, use_ws},
};

const RECENT_SCENE_LIMIT: usize = 6;

#[derive(Clone, Copy, PartialEq)]
enum CatalogResourceKind {
    Scene,
    Creature,
    Note,
    Class,
    Ability,
    Item,
}

impl CatalogResourceKind {
    fn label(self) -> &'static str {
        match self {
            Self::Scene => "Scene",
            Self::Creature => "Creature",
            Self::Note => "Note",
            Self::Class => "Class",
            Self::Ability => "Ability",
            Self::Item => "Item",
        }
    }

    fn plural(self) -> &'static str {
        match self {
            Self::Scene => "Scenes",
            Self::Creature => "Creatures",
            Self::Note => "Notes",
            Self::Class => "Classes",
            Self::Ability => "Abilities",
            Self::Item => "Items",
        }
    }

    fn marker(self) -> &'static str {
        match self {
            Self::Scene => "S",
            Self::Creature => "C",
            Self::Note => "N",
            Self::Class => "CL",
            Self::Ability => "A",
            Self::Item => "I",
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum CatalogCreateKind {
    Scene,
    Creature,
    Note,
    Class,
    Ability,
    Item,
}

impl CatalogCreateKind {
    fn value(self) -> &'static str {
        match self {
            Self::Scene => "scene",
            Self::Creature => "creature",
            Self::Note => "note",
            Self::Class => "class",
            Self::Ability => "ability",
            Self::Item => "item",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Scene => "Scene",
            Self::Creature => "Creature",
            Self::Note => "Note",
            Self::Class => "Class",
            Self::Ability => "Ability",
            Self::Item => "Item",
        }
    }

    fn from_value(value: &str) -> Self {
        match value {
            "creature" => Self::Creature,
            "note" => Self::Note,
            "class" => Self::Class,
            "ability" => Self::Ability,
            "item" => Self::Item,
            _ => Self::Scene,
        }
    }
}

#[derive(Clone, PartialEq)]
enum CatalogView {
    CurrentScene,
    Recent,
    Type(CatalogResourceKind),
    Collections,
    Collection(FolderPath),
}

#[derive(Clone, PartialEq)]
struct CatalogEntry {
    key: String,
    kind: CatalogResourceKind,
    name: String,
    detail: Option<String>,
    scene_id: Option<SceneID>,
}

#[derive(Clone)]
struct CreateCatalogResource {
    kind: CatalogCreateKind,
    name: String,
    background_image_url: String,
    class_id: Option<arptypes::ClassID>,
}

#[component]
pub fn CatalogPanel(
    game: Game,
    selected_scene_id: Option<SceneID>,
    on_select_scene: EventHandler<SceneID>,
) -> Element {
    let mut view = use_signal(|| CatalogView::CurrentScene);
    let mut search = use_signal(String::new);
    let mut show_create_modal = use_signal(|| false);
    let mut recent_scene_ids = use_signal(Vec::<SceneID>::new);

    let query = search().trim().to_lowercase();
    let searching = !query.is_empty();
    let title = if searching {
        "Search results".to_string()
    } else {
        catalog_view_title(&view())
    };

    let entries = if searching {
        all_catalog_entries(&game)
            .into_iter()
            .filter(|entry| catalog_entry_matches(entry, &query))
            .collect()
    } else {
        entries_for_view(&game, &view(), selected_scene_id, &recent_scene_ids())
    };

    let collection_paths = all_collection_paths(&game);
    let select_scene = {
        let on_select_scene = on_select_scene;
        Callback::new(move |scene_id| {
            let mut recent = recent_scene_ids();
            recent.retain(|id| *id != scene_id);
            recent.insert(0, scene_id);
            recent.truncate(RECENT_SCENE_LIMIT);
            recent_scene_ids.set(recent);
            on_select_scene.call(scene_id);
        })
    };

    rsx! {
        div {
            class: "flex h-full min-h-0 flex-col overflow-hidden rounded-lg border border-gray-200 bg-white shadow-sm",

            div {
                class: "border-b border-gray-200 p-4",
                div {
                    class: "flex items-start justify-between gap-3",
                    div {
                        h2 {
                            class: "text-lg font-semibold text-gray-900",
                            "Catalog"
                        }
                        p {
                            class: "mt-0.5 text-xs text-gray-500",
                            "Browse every resource in this game."
                        }
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        onclick: move |_| show_create_modal.set(true),
                        "+ New"
                    }
                }
                label {
                    class: "mt-3 block",
                    span { class: "sr-only", "Search the catalog" }
                    input {
                        class: "w-full rounded-md border border-gray-300 bg-gray-50 px-3 py-2 text-sm text-gray-900 outline-none focus:border-blue-500 focus:bg-white focus:ring-2 focus:ring-blue-100",
                        r#type: "search",
                        placeholder: "Search names and resource types...",
                        value: "{search}",
                        oninput: move |evt| search.set(evt.value()),
                    }
                }
            }

            div {
                class: "flex min-h-0 flex-1 overflow-hidden",
                nav {
                    class: "w-36 shrink-0 overflow-y-auto border-r border-gray-200 bg-gray-50 p-2",
                    "aria-label": "Catalog views",

                    CatalogNavButton {
                        label: "Current scene",
                        selected: !searching && view() == CatalogView::CurrentScene,
                        onclick: move |_| {
                            search.set(String::new());
                            view.set(CatalogView::CurrentScene);
                        },
                    }
                    CatalogNavButton {
                        label: "Recent",
                        selected: !searching && view() == CatalogView::Recent,
                        onclick: move |_| {
                            search.set(String::new());
                            view.set(CatalogView::Recent);
                        },
                    }

                    p {
                        class: "mb-1 mt-4 px-2 text-[0.65rem] font-semibold uppercase tracking-wider text-gray-400",
                        "Resources"
                    }
                    for kind in [
                        CatalogResourceKind::Scene,
                        CatalogResourceKind::Creature,
                        CatalogResourceKind::Note,
                    ] {
                        CatalogNavButton {
                            key: "{kind.label()}",
                            label: kind.plural(),
                            selected: !searching && view() == CatalogView::Type(kind),
                            onclick: move |_| {
                                search.set(String::new());
                                view.set(CatalogView::Type(kind));
                            },
                        }
                    }

                    p {
                        class: "mb-1 mt-4 px-2 text-[0.65rem] font-semibold uppercase tracking-wider text-gray-400",
                        "Rules"
                    }
                    for kind in [
                        CatalogResourceKind::Class,
                        CatalogResourceKind::Ability,
                        CatalogResourceKind::Item,
                    ] {
                        CatalogNavButton {
                            key: "{kind.label()}",
                            label: kind.plural(),
                            selected: !searching && view() == CatalogView::Type(kind),
                            onclick: move |_| {
                                search.set(String::new());
                                view.set(CatalogView::Type(kind));
                            },
                        }
                    }

                    p {
                        class: "mb-1 mt-4 px-2 text-[0.65rem] font-semibold uppercase tracking-wider text-gray-400",
                        "Collections"
                    }
                    CatalogNavButton {
                        label: "All collections",
                        selected: !searching && view() == CatalogView::Collections,
                        onclick: move |_| {
                            search.set(String::new());
                            view.set(CatalogView::Collections);
                        },
                    }
                    for path in collection_paths {
                        {
                            let label = collection_label(&path);
                            let selected = !searching
                                && view() == CatalogView::Collection(path.clone());
                            rsx! {
                                button {
                                    key: "{label}",
                                    r#type: "button",
                                    title: "{label}",
                                    class: if selected {
                                        "mb-0.5 block w-full truncate rounded bg-blue-100 px-2 py-1.5 text-left text-xs font-medium text-blue-800"
                                    } else {
                                        "mb-0.5 block w-full truncate rounded px-2 py-1.5 text-left text-xs text-gray-600 hover:bg-gray-200"
                                    },
                                    onclick: move |_| {
                                        search.set(String::new());
                                        view.set(CatalogView::Collection(path.clone()));
                                    },
                                    "{label}"
                                }
                            }
                        }
                    }

                }

                section {
                    class: "min-w-0 flex-1 overflow-y-auto p-3",
                    div {
                        class: "mb-3 flex items-center justify-between gap-2",
                        h3 {
                            class: "truncate text-sm font-semibold text-gray-800",
                            "{title}"
                        }
                        span {
                            class: "shrink-0 text-xs text-gray-400",
                            "{entries.len()} items"
                        }
                    }

                    if !searching && view() == CatalogView::Collections {
                        CollectionsOverview {
                            game: game.clone(),
                            on_open: move |path| view.set(CatalogView::Collection(path)),
                        }
                    } else if entries.is_empty() {
                        CatalogEmptyState {
                            searching,
                            view: view(),
                        }
                    } else {
                        div {
                            class: "space-y-1.5",
                            for entry in entries {
                                CatalogResourceRow {
                                    key: "{entry.key}",
                                    entry,
                                    selected_scene_id,
                                    on_select_scene: select_scene,
                                }
                            }
                        }
                    }
                }
            }
        }

        if show_create_modal() {
            CreateCatalogResourceModal {
                game: game.clone(),
                on_close: move |_| show_create_modal.set(false),
            }
        }
    }
}

#[component]
fn CatalogNavButton(
    label: &'static str,
    selected: bool,
    onclick: EventHandler<MouseEvent>,
) -> Element {
    rsx! {
        button {
            r#type: "button",
            class: if selected {
                "mb-0.5 block w-full rounded bg-blue-600 px-2 py-1.5 text-left text-xs font-medium text-white"
            } else {
                "mb-0.5 block w-full rounded px-2 py-1.5 text-left text-xs text-gray-700 hover:bg-gray-200"
            },
            onclick,
            "{label}"
        }
    }
}

#[component]
fn CatalogResourceRow(
    entry: CatalogEntry,
    selected_scene_id: Option<SceneID>,
    on_select_scene: EventHandler<SceneID>,
) -> Element {
    let selected = entry.scene_id.is_some() && entry.scene_id == selected_scene_id;
    let row_class = if selected {
        "flex w-full items-center gap-3 rounded-md border border-blue-200 bg-blue-50 px-3 py-2 text-left"
    } else {
        "flex w-full items-center gap-3 rounded-md border border-gray-200 bg-white px-3 py-2 text-left hover:border-gray-300 hover:bg-gray-50"
    };

    let content = rsx! {
        span {
            class: if selected {
                "flex h-8 w-8 shrink-0 items-center justify-center rounded bg-blue-600 text-[0.65rem] font-bold text-white"
            } else {
                "flex h-8 w-8 shrink-0 items-center justify-center rounded bg-gray-100 text-[0.65rem] font-bold text-gray-500"
            },
            "{entry.kind.marker()}"
        }
        span {
            class: "min-w-0 flex-1",
            span {
                class: if selected {
                    "block truncate text-sm font-medium text-blue-900"
                } else {
                    "block truncate text-sm font-medium text-gray-800"
                },
                "{entry.name}"
            }
            span {
                class: "block truncate text-xs text-gray-500",
                "{entry.kind.label()}"
                if let Some(detail) = &entry.detail {
                    " · {detail}"
                }
            }
        }
    };

    if let Some(scene_id) = entry.scene_id {
        rsx! {
            button {
                r#type: "button",
                class: "{row_class}",
                onclick: move |_| on_select_scene.call(scene_id),
                {content}
            }
        }
    } else {
        rsx! {
            div {
                class: "{row_class}",
                {content}
            }
        }
    }
}

#[component]
fn CatalogEmptyState(searching: bool, view: CatalogView) -> Element {
    let message = if searching {
        "No resources match this search."
    } else {
        match view {
            CatalogView::CurrentScene => "No scene is currently selected.",
            CatalogView::Recent => "Scenes you open will appear here for this session.",
            CatalogView::Collections => "There are no collections yet.",
            _ => "There are no resources in this view yet.",
        }
    };

    rsx! {
        div {
            class: "rounded-lg border border-dashed border-gray-300 px-4 py-8 text-center",
            p { class: "text-sm text-gray-500", "{message}" }
        }
    }
}

#[component]
fn CollectionsOverview(game: Game, on_open: EventHandler<FolderPath>) -> Element {
    let paths = all_collection_paths(&game);

    rsx! {
        div {
            class: "space-y-2",
            for path in paths {
                {
                    let label = collection_label(&path);
                    let count = game
                        .campaign
                        .get(&path)
                        .map(folder_resource_count)
                        .unwrap_or_default();
                    rsx! {
                        button {
                            key: "{label}",
                            r#type: "button",
                            class: "block w-full rounded-md border border-gray-200 bg-white px-3 py-3 text-left hover:border-blue-200 hover:bg-blue-50",
                            onclick: move |_| on_open.call(path.clone()),
                            span {
                                class: "block truncate text-sm font-medium text-gray-800",
                                "{label}"
                            }
                            span {
                                class: "mt-0.5 block text-xs text-gray-500",
                                "{count} resources"
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn CreateCatalogResourceModal(game: Game, on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let mut kind = use_signal(|| CatalogCreateKind::Scene);
    let mut name = use_signal(String::new);
    let mut background_image_url = use_signal(String::new);
    let first_class_id = game.classes.values().next().map(|class| class.id);
    let mut selected_class_id = use_signal(move || first_class_id);

    let mut classes: Vec<_> = game
        .classes
        .values()
        .map(|class| (class.id, class.name.clone()))
        .collect();
    classes.sort_by(|a, b| a.1.cmp(&b.1));

    let mut create_action = use_action({
        let game = game.clone();
        let on_close = on_close;
        move |request: CreateCatalogResource| {
            let ws = ws;
            let game = game.clone();
            let on_close = on_close;
            async move {
                let path = catalog_folder_path();
                if game.campaign.get(&path).is_err() {
                    send_catalog_command(ws, GMCommand::CreateFolder { path: path.clone() })
                        .await?;
                }

                let command = create_catalog_command(request, path)?;
                send_catalog_command(ws, command).await?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });

    let current_kind = kind();
    let trimmed_name = name().trim().to_string();
    let can_submit = !trimmed_name.is_empty()
        && (current_kind != CatalogCreateKind::Creature || selected_class_id().is_some());

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            h3 {
                class: "text-lg font-semibold text-gray-900",
                "Create catalog resource"
            }
            p {
                class: "mt-1 text-sm text-gray-500",
                "New resources are added to the catalog automatically."
            }

            form {
                class: "mt-5 space-y-4",
                onsubmit: move |evt| {
                    evt.prevent_default();
                    if can_submit {
                        create_action.call(CreateCatalogResource {
                            kind: current_kind,
                            name: trimmed_name.clone(),
                            background_image_url: background_image_url().trim().to_string(),
                            class_id: selected_class_id(),
                        });
                    }
                },

                label {
                    class: "block",
                    span {
                        class: "mb-1 block text-sm font-medium text-gray-700",
                        "Resource type"
                    }
                    select {
                        class: "w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm",
                        value: "{current_kind.value()}",
                        oninput: move |evt| kind.set(CatalogCreateKind::from_value(&evt.value())),
                        for option_kind in [
                            CatalogCreateKind::Scene,
                            CatalogCreateKind::Creature,
                            CatalogCreateKind::Note,
                            CatalogCreateKind::Class,
                            CatalogCreateKind::Ability,
                            CatalogCreateKind::Item,
                        ] {
                            option {
                                key: "{option_kind.value()}",
                                value: "{option_kind.value()}",
                                "{option_kind.label()}"
                            }
                        }
                    }
                }

                label {
                    class: "block",
                    span {
                        class: "mb-1 block text-sm font-medium text-gray-700",
                        "Name"
                    }
                    input {
                        class: "w-full rounded-md border border-gray-300 px-3 py-2 text-sm",
                        r#type: "text",
                        autofocus: true,
                        placeholder: "Name",
                        value: "{name}",
                        oninput: move |evt| name.set(evt.value()),
                    }
                }

                if current_kind == CatalogCreateKind::Scene {
                    label {
                        class: "block",
                        span {
                            class: "mb-1 block text-sm font-medium text-gray-700",
                            "Background image URL"
                        }
                        input {
                            class: "w-full rounded-md border border-gray-300 px-3 py-2 text-sm",
                            r#type: "url",
                            placeholder: "Optional",
                            value: "{background_image_url}",
                            oninput: move |evt| background_image_url.set(evt.value()),
                        }
                    }
                }

                if current_kind == CatalogCreateKind::Creature {
                    label {
                        class: "block",
                        span {
                            class: "mb-1 block text-sm font-medium text-gray-700",
                            "Class"
                        }
                        select {
                            class: "w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm",
                            value: selected_class_id().map(|id| id.to_string()).unwrap_or_default(),
                            oninput: move |evt| {
                                let value = evt.value();
                                selected_class_id.set(
                                    classes
                                        .iter()
                                        .find(|(id, _)| id.to_string() == value)
                                        .map(|(id, _)| *id),
                                );
                            },
                            if classes.is_empty() {
                                option { value: "", "Create a class first" }
                            } else {
                                for (class_id, class_name) in &classes {
                                    option {
                                        key: "{class_id}",
                                        value: "{class_id}",
                                        "{class_name}"
                                    }
                                }
                            }
                        }
                    }
                }

                if let Some(Err(err)) = create_action.value() {
                    p {
                        class: "rounded bg-red-50 px-3 py-2 text-sm text-red-700",
                        "Creation failed: {err}"
                    }
                }

                div {
                    class: "flex justify-end gap-2 pt-2",
                    Button {
                        r#type: "button",
                        variant: ButtonVariant::Ghost,
                        onclick: move |_| on_close.call(()),
                        "Cancel"
                    }
                    Button {
                        r#type: "submit",
                        variant: ButtonVariant::Primary,
                        disabled: !can_submit || create_action.pending(),
                        if create_action.pending() {
                            "Creating..."
                        } else {
                            "Create"
                        }
                    }
                }
            }
        }
    }
}

async fn send_catalog_command(
    ws: Coroutine<crate::rpi::UIRequest>,
    command: GMCommand,
) -> anyhow::Result<()> {
    let result = send_request::<Result<Vec<arptypes::GameLog>, String>>(
        RPIGameRequest::GMCommand {
            command: Box::new(command),
        },
        ws,
    )
    .await?;
    result.map(|_| ()).map_err(anyhow::Error::msg)
}

fn create_catalog_command(
    request: CreateCatalogResource,
    path: FolderPath,
) -> anyhow::Result<GMCommand> {
    let command = match request.kind {
        CatalogCreateKind::Scene => GMCommand::CreateScene {
            path,
            scene: SceneCreation {
                name: request.name,
                background_image_url: request.background_image_url,
                background_image_offset: None,
                background_image_scale: (1.0, 1.0),
            },
        },
        CatalogCreateKind::Creature => GMCommand::CreateCreature {
            path,
            creature: CreatureCreation {
                name: request.name,
                class: request
                    .class_id
                    .ok_or_else(|| anyhow::anyhow!("A creature requires a class."))?,
                portrait_url: String::new(),
                icon_url: String::new(),
                note: String::new(),
                bio: String::new(),
                initiative: Dice::Expr { num: 1, size: 20 },
                size: AABB {
                    x: u32meter(1u32),
                    y: u32meter(1u32),
                    z: u32meter(1u32),
                },
            },
        },
        CatalogCreateKind::Note => GMCommand::CreateNote {
            path,
            note: Note {
                name: request.name,
                content: String::new(),
            },
        },
        CatalogCreateKind::Class => GMCommand::CreateClass {
            path,
            class: ClassCreation {
                name: request.name,
                abilities: Vec::new(),
                conditions: Vec::new(),
                color: "white".to_string(),
                emoji: None,
            },
        },
        CatalogCreateKind::Ability => GMCommand::CreateAbility {
            path,
            ability: AbilityCreation {
                name: request.name,
                cost: Energy(0),
                action: Action::Creature {
                    effect: CreatureEffect::Damage(Dice::Flat { value: 1 }),
                    target: CreatureTarget::Melee,
                },
                usable_ooc: false,
            },
        },
        CatalogCreateKind::Item => GMCommand::CreateItem {
            path,
            name: request.name,
        },
    };
    Ok(command)
}

fn catalog_folder_path() -> FolderPath {
    FolderPath::from_vec(vec!["catalog".to_string()])
}

fn catalog_view_title(view: &CatalogView) -> String {
    match view {
        CatalogView::CurrentScene => "Current scene".to_string(),
        CatalogView::Recent => "Recent".to_string(),
        CatalogView::Type(kind) => kind.plural().to_string(),
        CatalogView::Collections => "Collections".to_string(),
        CatalogView::Collection(path) => collection_label(path),
    }
}

fn entries_for_view(
    game: &Game,
    view: &CatalogView,
    selected_scene_id: Option<SceneID>,
    recent_scene_ids: &[SceneID],
) -> Vec<CatalogEntry> {
    match view {
        CatalogView::CurrentScene => selected_scene_id
            .and_then(|id| game.scenes.get(&id))
            .map(scene_entry)
            .into_iter()
            .collect(),
        CatalogView::Recent => recent_scene_ids
            .iter()
            .filter_map(|id| game.scenes.get(id))
            .map(scene_entry)
            .collect(),
        CatalogView::Type(kind) => all_catalog_entries(game)
            .into_iter()
            .filter(|entry| entry.kind == *kind)
            .collect(),
        CatalogView::Collection(path) => collection_entries(game, path),
        CatalogView::Collections => Vec::new(),
    }
}

fn all_catalog_entries(game: &Game) -> Vec<CatalogEntry> {
    let mut entries = Vec::new();

    entries.extend(game.scenes.values().map(scene_entry));
    entries.extend(game.creatures.values().map(|creature| CatalogEntry {
        key: format!("creature:{}", creature.id),
        kind: CatalogResourceKind::Creature,
        name: creature.name.clone(),
        detail: None,
        scene_id: None,
    }));
    entries.extend(game.classes.values().map(|class| CatalogEntry {
        key: format!("class:{}", class.id),
        kind: CatalogResourceKind::Class,
        name: class.name.clone(),
        detail: None,
        scene_id: None,
    }));
    entries.extend(game.abilities.values().map(|ability| CatalogEntry {
        key: format!("ability:{}", ability.id),
        kind: CatalogResourceKind::Ability,
        name: ability.name.clone(),
        detail: None,
        scene_id: None,
    }));
    entries.extend(game.items.values().map(|item| CatalogEntry {
        key: format!("item:{}", item.id),
        kind: CatalogResourceKind::Item,
        name: item.name.clone(),
        detail: None,
        scene_id: None,
    }));

    for path in game.campaign.walk_paths(&FolderPath::root()) {
        let Ok(folder) = game.campaign.get(path) else {
            continue;
        };
        entries.extend(folder.notes.values().map(|note| CatalogEntry {
            key: format!("note:{}:{}", collection_label(path), note.name),
            kind: CatalogResourceKind::Note,
            name: note.name.clone(),
            detail: Some(collection_label(path)),
            scene_id: None,
        }));
    }

    sort_catalog_entries(&mut entries);
    entries
}

fn collection_entries(game: &Game, path: &FolderPath) -> Vec<CatalogEntry> {
    let Ok(folder) = game.campaign.get(path) else {
        return Vec::new();
    };
    let detail = Some(collection_label(path));
    let mut entries = Vec::new();

    entries.extend(folder.scenes.iter().filter_map(|id| {
        game.scenes.get(id).map(|scene| {
            let mut entry = scene_entry(scene);
            entry.detail = detail.clone();
            entry
        })
    }));
    entries.extend(folder.creatures.iter().filter_map(|id| {
        game.creatures.get(id).map(|creature| CatalogEntry {
            key: format!("creature:{}", creature.id),
            kind: CatalogResourceKind::Creature,
            name: creature.name.clone(),
            detail: detail.clone(),
            scene_id: None,
        })
    }));
    entries.extend(folder.notes.values().map(|note| CatalogEntry {
        key: format!("note:{}:{}", collection_label(path), note.name),
        kind: CatalogResourceKind::Note,
        name: note.name.clone(),
        detail: detail.clone(),
        scene_id: None,
    }));
    entries.extend(folder.classes.iter().filter_map(|id| {
        game.classes.get(id).map(|class| CatalogEntry {
            key: format!("class:{}", class.id),
            kind: CatalogResourceKind::Class,
            name: class.name.clone(),
            detail: detail.clone(),
            scene_id: None,
        })
    }));
    entries.extend(folder.abilities.iter().filter_map(|id| {
        game.abilities.get(id).map(|ability| CatalogEntry {
            key: format!("ability:{}", ability.id),
            kind: CatalogResourceKind::Ability,
            name: ability.name.clone(),
            detail: detail.clone(),
            scene_id: None,
        })
    }));
    entries.extend(folder.items.iter().filter_map(|id| {
        game.items.get(id).map(|item| CatalogEntry {
            key: format!("item:{}", item.id),
            kind: CatalogResourceKind::Item,
            name: item.name.clone(),
            detail: detail.clone(),
            scene_id: None,
        })
    }));

    sort_catalog_entries(&mut entries);
    entries
}

fn scene_entry(scene: &arptypes::Scene) -> CatalogEntry {
    CatalogEntry {
        key: format!("scene:{}", scene.id),
        kind: CatalogResourceKind::Scene,
        name: scene.name.clone(),
        detail: None,
        scene_id: Some(scene.id),
    }
}

fn sort_catalog_entries(entries: &mut [CatalogEntry]) {
    entries.sort_by(|a, b| {
        a.kind
            .label()
            .cmp(b.kind.label())
            .then_with(|| a.name.to_lowercase().cmp(&b.name.to_lowercase()))
            .then_with(|| a.key.cmp(&b.key))
    });
}

fn catalog_entry_matches(entry: &CatalogEntry, normalized_query: &str) -> bool {
    entry.name.to_lowercase().contains(normalized_query)
        || entry.kind.label().to_lowercase().contains(normalized_query)
        || entry
            .kind
            .plural()
            .to_lowercase()
            .contains(normalized_query)
}

fn all_collection_paths(game: &Game) -> Vec<FolderPath> {
    let mut paths: Vec<_> = game
        .campaign
        .walk_paths(&FolderPath::root())
        .cloned()
        .collect();
    paths.sort_by_key(collection_label);
    paths
}

fn collection_label(path: &FolderPath) -> String {
    if path.is_root() {
        "/".to_string()
    } else {
        path.to_string()
    }
}

fn folder_resource_count(folder: &Folder) -> usize {
    folder.scenes.len()
        + folder.creatures.len()
        + folder.notes.len()
        + folder.items.len()
        + folder.abilities.len()
        + folder.classes.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creation_uses_catalog_folder() {
        assert_eq!(catalog_folder_path().to_string(), "/catalog");
    }

    #[test]
    fn search_matches_name_and_resource_type() {
        let entry = CatalogEntry {
            key: "scene:1".to_string(),
            kind: CatalogResourceKind::Scene,
            name: "Moonlit Harbor".to_string(),
            detail: None,
            scene_id: None,
        };

        assert!(catalog_entry_matches(&entry, "moonlit"));
        assert!(catalog_entry_matches(&entry, "scene"));
        assert!(catalog_entry_matches(&entry, "scenes"));
        assert!(!catalog_entry_matches(&entry, "creature"));
    }
}
