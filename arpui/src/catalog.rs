use std::collections::HashSet;

use arptypes::{
    AABB, AbilityCreation, Action, ClassCreation, Collection, CollectionID, CollectionResources,
    CreatureCreation, CreatureEffect, CreatureTarget, Dice, Energy, GMCommand, Game,
    NoteVisibility, ResourceRef, SceneCreation, SceneID, multitenant::RPIGameRequest, u32meter,
};
use dioxus::prelude::*;

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
    Collection(CollectionID),
}

#[derive(Clone, PartialEq)]
struct CatalogEntry {
    key: String,
    resource: ResourceRef,
    kind: CatalogResourceKind,
    name: String,
    detail: Option<String>,
    scene_id: Option<SceneID>,
    icon_url: Option<String>,
    emoji: Option<String>,
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
    let mut show_create_collection_modal = use_signal(|| false);
    let mut manage_collection_id = use_signal(|| None::<CollectionID>);
    let mut manage_resource = use_signal(|| None::<ResourceRef>);
    let mut recent_scene_ids = use_signal(Vec::<SceneID>::new);

    let query = search().trim().to_lowercase();
    let searching = !query.is_empty();
    let title = if searching {
        "Search results".to_string()
    } else {
        catalog_view_title(&game, &view())
    };

    let entries = if searching {
        all_catalog_entries(&game)
            .into_iter()
            .filter(|entry| catalog_entry_matches(entry, &query))
            .collect()
    } else {
        entries_for_view(&game, &view(), selected_scene_id, &recent_scene_ids())
    };

    let collections = all_collections(&game);
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
                    div {
                        class: "flex gap-2",
                        Button {
                            variant: ButtonVariant::Outline,
                            onclick: move |_| show_create_collection_modal.set(true),
                            "+ Collection"
                        }
                        Button {
                            variant: ButtonVariant::Primary,
                            onclick: move |_| show_create_modal.set(true),
                            "+ Resource"
                        }
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
                    for collection in collections {
                        {
                            let collection_id = collection.id;
                            let label = collection.name;
                            let selected = !searching
                                && view() == CatalogView::Collection(collection_id);
                            rsx! {
                                button {
                                    key: "{collection_id}",
                                    r#type: "button",
                                    title: "{label}",
                                    class: if selected {
                                        "mb-0.5 block w-full truncate rounded bg-blue-100 px-2 py-1.5 text-left text-xs font-medium text-blue-800"
                                    } else {
                                        "mb-0.5 block w-full truncate rounded px-2 py-1.5 text-left text-xs text-gray-600 hover:bg-gray-200"
                                    },
                                    onclick: move |_| {
                                        search.set(String::new());
                                        view.set(CatalogView::Collection(collection_id));
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
                        div {
                            class: "flex shrink-0 items-center gap-2",
                            span {
                                class: "text-xs text-gray-400",
                                "{entries.len()} items"
                            }
                            if let CatalogView::Collection(collection_id) = view() {
                                Button {
                                    variant: ButtonVariant::Outline,
                                    onclick: move |_| manage_collection_id.set(Some(collection_id)),
                                    "Manage"
                                }
                            }
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
                                    on_manage: move |resource| manage_resource.set(Some(resource)),
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

        if show_create_collection_modal() {
            CreateCollectionModal {
                on_close: move |_| show_create_collection_modal.set(false),
            }
        }

        if let Some(collection_id) = manage_collection_id() {
            ManageCollectionModal {
                game: game.clone(),
                collection_id,
                on_close: move |_| manage_collection_id.set(None),
                on_navigate: move |destination: Option<CollectionID>| {
                    manage_collection_id.set(None);
                    search.set(String::new());
                    view.set(destination
                        .map(CatalogView::Collection)
                        .unwrap_or(CatalogView::Collections));
                },
            }
        }

        if let Some(resource) = manage_resource() {
            ManageResourceModal {
                game: game.clone(),
                resource,
                on_close: move |_| manage_resource.set(None),
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
    on_manage: EventHandler<ResourceRef>,
) -> Element {
    let selected = entry.scene_id.is_some() && entry.scene_id == selected_scene_id;
    let row_class = if selected {
        "flex w-full items-center gap-3 rounded-md border border-blue-200 bg-blue-50 px-3 py-2 text-left"
    } else {
        "flex w-full items-center gap-3 rounded-md border border-gray-200 bg-white px-3 py-2 text-left hover:border-gray-300 hover:bg-gray-50"
    };

    let icon = rsx! {
        if let Some(icon_url) = &entry.icon_url {
            img {
                class: "h-8 w-8 shrink-0 rounded object-cover",
                src: "{icon_url}",
                alt: "",
            }
        } else if let Some(emoji) = &entry.emoji {
            span {
                class: "flex h-8 w-8 shrink-0 items-center justify-center text-xl",
                title: "{entry.name}",
                "{emoji}"
            }
        } else {
            span {
                class: if selected {
                    "flex h-8 w-8 shrink-0 items-center justify-center rounded bg-blue-600 text-[0.65rem] font-bold text-white"
                } else {
                    "flex h-8 w-8 shrink-0 items-center justify-center rounded bg-gray-100 text-[0.65rem] font-bold text-gray-500"
                },
                "{entry.kind.marker()}"
            }
        }
    };
    let description = rsx! {
        span {
            class: "block min-w-0",
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
                    span {
                        title: "{detail}",
                        " · {detail}"
                    }
                }
            }
        }
    };

    rsx! {
        div {
            class: "{row_class}",
            {icon}
            if let Some(scene_id) = entry.scene_id {
                button {
                    r#type: "button",
                    class: "min-w-0 flex-1 text-left",
                    onclick: move |_| on_select_scene.call(scene_id),
                    {description}
                }
            } else {
                span {
                    class: "min-w-0 flex-1",
                    {description}
                }
            }
            button {
                r#type: "button",
                class: "shrink-0 rounded px-2 py-1 text-xs font-medium text-gray-500 hover:bg-gray-200 hover:text-gray-800",
                title: "Manage collections or delete from catalog",
                onclick: move |_| on_manage.call(entry.resource),
                "Manage"
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
fn CollectionsOverview(game: Game, on_open: EventHandler<CollectionID>) -> Element {
    let collections = all_collections(&game);

    rsx! {
        div {
            class: "space-y-2",
            for collection in collections {
                {
                    let collection_id = collection.id;
                    let count = collection_resource_count(&collection);
                    let label = collection.name;
                    rsx! {
                        button {
                            key: "{collection_id}",
                            r#type: "button",
                            class: "block w-full rounded-md border border-gray-200 bg-white px-3 py-3 text-left hover:border-blue-200 hover:bg-blue-50",
                            onclick: move |_| on_open.call(collection_id),
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
        let on_close = on_close;
        move |request: CreateCatalogResource| {
            let ws = ws;
            let on_close = on_close;
            async move {
                let command = create_catalog_command(request)?;
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

#[component]
fn CreateCollectionModal(on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let mut name = use_signal(String::new);
    let mut create_action = use_action({
        let on_close = on_close;
        move |name: String| {
            let on_close = on_close;
            async move {
                send_catalog_command(ws, GMCommand::CreateCollection { name }).await?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });
    let trimmed_name = name().trim().to_string();

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            h3 {
                class: "text-lg font-semibold text-gray-900",
                "Create collection"
            }
            p {
                class: "mt-1 text-sm text-gray-500",
                "Collections organize existing catalog resources without moving or copying them."
            }
            form {
                class: "mt-5 space-y-4",
                onsubmit: move |evt| {
                    evt.prevent_default();
                    if !trimmed_name.is_empty() {
                        create_action.call(trimmed_name.clone());
                    }
                },
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
                        value: "{name}",
                        oninput: move |evt| name.set(evt.value()),
                    }
                }
                if let Some(Err(error)) = create_action.value() {
                    p {
                        class: "rounded bg-red-50 px-3 py-2 text-sm text-red-700",
                        "Creation failed: {error}"
                    }
                }
                div {
                    class: "flex justify-end gap-2",
                    Button {
                        r#type: "button",
                        variant: ButtonVariant::Ghost,
                        onclick: move |_| on_close.call(()),
                        "Cancel"
                    }
                    Button {
                        r#type: "submit",
                        variant: ButtonVariant::Primary,
                        disabled: trimmed_name.is_empty() || create_action.pending(),
                        if create_action.pending() { "Creating..." } else { "Create" }
                    }
                }
            }
        }
    }
}

#[component]
fn ManageCollectionModal(
    game: Game,
    collection_id: CollectionID,
    on_close: EventHandler<()>,
    on_navigate: EventHandler<Option<CollectionID>>,
) -> Element {
    let ws = use_ws();
    let Some(collection) = game.collections.get(&collection_id).cloned() else {
        return rsx! {
            Modal {
                open: true,
                on_close: move |_| on_close.call(()),
                class: "p-6",
                p { class: "text-sm text-red-700", "Collection no longer exists." }
            }
        };
    };

    let initial_name = collection.name.clone();
    let mut name = use_signal(move || initial_name);
    let destinations: Vec<_> = all_collections(&game)
        .into_iter()
        .filter(|candidate| candidate.id != collection_id)
        .collect();
    let first_destination = destinations.first().map(|candidate| candidate.id);
    let mut merge_destination = use_signal(move || first_destination);
    let mut confirm_delete = use_signal(|| false);

    let mut rename_action = use_action({
        let on_close = on_close;
        move |name: String| {
            let on_close = on_close;
            async move {
                send_catalog_command(
                    ws,
                    GMCommand::RenameCollection {
                        collection_id,
                        name,
                    },
                )
                .await?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });
    let mut merge_action = use_action({
        let on_navigate = on_navigate;
        move |destination_id: CollectionID| {
            let on_navigate = on_navigate;
            async move {
                send_catalog_command(
                    ws,
                    GMCommand::MergeCollections {
                        destination_id,
                        source_ids: vec![collection_id],
                    },
                )
                .await?;
                on_navigate.call(Some(destination_id));
                Ok::<(), anyhow::Error>(())
            }
        }
    });
    let mut delete_action = use_action({
        let on_navigate = on_navigate;
        move |_| {
            let on_navigate = on_navigate;
            async move {
                send_catalog_command(ws, GMCommand::DeleteCollection { collection_id }).await?;
                on_navigate.call(None);
                Ok::<(), anyhow::Error>(())
            }
        }
    });
    let trimmed_name = name().trim().to_string();
    let pending = rename_action.pending() || merge_action.pending() || delete_action.pending();

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6 max-w-xl",
            h3 {
                class: "text-lg font-semibold text-gray-900",
                "Manage collection"
            }
            p {
                class: "mt-1 text-sm text-gray-500",
                "{collection_resource_count(&collection)} resources"
            }

            form {
                class: "mt-5",
                onsubmit: move |evt| {
                    evt.prevent_default();
                    if !trimmed_name.is_empty() && trimmed_name != collection.name {
                        rename_action.call(trimmed_name.clone());
                    }
                },
                label {
                    class: "block",
                    span {
                        class: "mb-1 block text-sm font-medium text-gray-700",
                        "Name"
                    }
                    div {
                        class: "flex gap-2",
                        input {
                            class: "min-w-0 flex-1 rounded-md border border-gray-300 px-3 py-2 text-sm",
                            r#type: "text",
                            value: "{name}",
                            oninput: move |evt| name.set(evt.value()),
                        }
                        Button {
                            r#type: "submit",
                            variant: ButtonVariant::Primary,
                            disabled: pending || trimmed_name.is_empty() || trimmed_name == collection.name,
                            if rename_action.pending() { "Saving..." } else { "Rename" }
                        }
                    }
                }
            }

            div {
                class: "mt-6 border-t border-gray-200 pt-5",
                h4 { class: "text-sm font-semibold text-gray-900", "Merge collection" }
                p {
                    class: "mt-1 text-xs text-gray-500",
                    "Move every membership into another collection, then remove this collection. Catalog resources are preserved."
                }
                if destinations.is_empty() {
                    p { class: "mt-3 text-sm text-gray-500", "Create another collection before merging." }
                } else {
                    div {
                        class: "mt-3 flex gap-2",
                        select {
                            class: "min-w-0 flex-1 rounded-md border border-gray-300 bg-white px-3 py-2 text-sm",
                            value: merge_destination().map(|id| id.to_string()).unwrap_or_default(),
                            oninput: move |evt| {
                                let value = evt.value();
                                merge_destination.set(
                                    destinations
                                        .iter()
                                        .find(|candidate| candidate.id.to_string() == value)
                                        .map(|candidate| candidate.id),
                                );
                            },
                            for destination in &destinations {
                                option {
                                    key: "{destination.id}",
                                    value: "{destination.id}",
                                    "{destination.name}"
                                }
                            }
                        }
                        Button {
                            variant: ButtonVariant::Outline,
                            disabled: pending || merge_destination().is_none(),
                            onclick: move |_| {
                                if let Some(destination_id) = merge_destination() {
                                    merge_action.call(destination_id);
                                }
                            },
                            if merge_action.pending() { "Merging..." } else { "Merge into" }
                        }
                    }
                }
            }

            div {
                class: "mt-6 border-t border-red-100 pt-5",
                h4 { class: "text-sm font-semibold text-red-800", "Delete collection" }
                p {
                    class: "mt-1 text-xs text-gray-500",
                    "Deleting a collection does not delete any catalog resources."
                }
                if confirm_delete() {
                    div {
                        class: "mt-3 flex items-center justify-between gap-3 rounded bg-red-50 p-3",
                        span { class: "text-sm text-red-800", "Delete this collection?" }
                        div {
                            class: "flex gap-2",
                            Button {
                                variant: ButtonVariant::Ghost,
                                disabled: pending,
                                onclick: move |_| confirm_delete.set(false),
                                "Cancel"
                            }
                            Button {
                                variant: ButtonVariant::Destructive,
                                disabled: pending,
                                onclick: move |_| delete_action.call(()),
                                if delete_action.pending() { "Deleting..." } else { "Delete" }
                            }
                        }
                    }
                } else {
                    Button {
                        variant: ButtonVariant::Destructive,
                        disabled: pending,
                        onclick: move |_| confirm_delete.set(true),
                        "Delete collection"
                    }
                }
            }

            for result in [rename_action.value(), merge_action.value(), delete_action.value()] {
                if let Some(Err(error)) = result {
                    p {
                        class: "mt-4 rounded bg-red-50 px-3 py-2 text-sm text-red-700",
                        "Operation failed: {error}"
                    }
                }
            }

            div {
                class: "mt-6 flex justify-end",
                Button {
                    variant: ButtonVariant::Ghost,
                    disabled: pending,
                    onclick: move |_| on_close.call(()),
                    "Close"
                }
            }
        }
    }
}

#[component]
fn ManageResourceModal(game: Game, resource: ResourceRef, on_close: EventHandler<()>) -> Element {
    let ws = use_ws();
    let Some(resource_name) = resource_name(&game, resource) else {
        return rsx! {
            Modal {
                open: true,
                on_close: move |_| on_close.call(()),
                class: "p-6",
                p { class: "text-sm text-red-700", "Resource no longer exists." }
            }
        };
    };
    let collections = all_collections(&game);
    let original: HashSet<_> = collections
        .iter()
        .filter(|collection| collection_contains_resource(collection, resource))
        .map(|collection| collection.id)
        .collect();
    let initial_selected = original.clone();
    let mut selected = use_signal(move || initial_selected);
    let mut confirm_delete = use_signal(|| false);

    let mut membership_action = use_action({
        let original = original.clone();
        let on_close = on_close;
        move |updated: HashSet<CollectionID>| {
            let original = original.clone();
            let on_close = on_close;
            async move {
                let mut additions: Vec<_> = updated.difference(&original).copied().collect();
                let mut removals: Vec<_> = original.difference(&updated).copied().collect();
                additions.sort();
                removals.sort();
                for collection_id in removals {
                    send_catalog_command(
                        ws,
                        GMCommand::RemoveResourcesFromCollection {
                            collection_id,
                            resources: collection_resources(resource),
                        },
                    )
                    .await?;
                }
                for collection_id in additions {
                    send_catalog_command(
                        ws,
                        GMCommand::AddResourcesToCollection {
                            collection_id,
                            resources: collection_resources(resource),
                        },
                    )
                    .await?;
                }
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });
    let mut delete_action = use_action({
        let on_close = on_close;
        move |_| {
            let on_close = on_close;
            async move {
                send_catalog_command(ws, GMCommand::DeleteResource { resource }).await?;
                on_close.call(());
                Ok::<(), anyhow::Error>(())
            }
        }
    });
    let pending = membership_action.pending() || delete_action.pending();

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6 max-w-xl",
            h3 {
                class: "text-lg font-semibold text-gray-900",
                "Manage {resource_name}"
            }
            p {
                class: "mt-1 text-sm text-gray-500",
                "Choose every collection where this resource should appear."
            }

            div {
                class: "mt-5 max-h-72 overflow-y-auto rounded-md border border-gray-200",
                if collections.is_empty() {
                    p {
                        class: "p-4 text-sm text-gray-500",
                        "There are no collections yet."
                    }
                } else {
                    for collection in &collections {
                        {
                            let collection_id = collection.id;
                            rsx! {
                                label {
                                    key: "{collection_id}",
                                    class: "flex cursor-pointer items-center justify-between gap-3 border-b border-gray-100 px-3 py-2 text-sm last:border-b-0 hover:bg-gray-50",
                                    span { class: "truncate text-gray-800", "{collection.name}" }
                                    input {
                                        r#type: "checkbox",
                                        checked: selected().contains(&collection_id),
                                        onchange: move |evt| {
                                            selected.with_mut(|selected| {
                                                if evt.checked() {
                                                    selected.insert(collection_id);
                                                } else {
                                                    selected.remove(&collection_id);
                                                }
                                            });
                                        },
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if let Some(Err(error)) = membership_action.value() {
                p {
                    class: "mt-4 rounded bg-red-50 px-3 py-2 text-sm text-red-700",
                    "Membership update failed: {error}"
                }
            }
            if let Some(Err(error)) = delete_action.value() {
                p {
                    class: "mt-4 rounded bg-red-50 px-3 py-2 text-sm text-red-700",
                    "Catalog deletion failed: {error}"
                }
            }

            div {
                class: "mt-5 flex items-center justify-between gap-3",
                div {
                    if confirm_delete() {
                        div {
                            class: "flex items-center gap-2 rounded bg-red-50 p-2",
                            span { class: "text-xs text-red-800", "Delete from the catalog everywhere?" }
                            Button {
                                variant: ButtonVariant::Destructive,
                                disabled: pending,
                                onclick: move |_| delete_action.call(()),
                                if delete_action.pending() { "Deleting..." } else { "Confirm" }
                            }
                            Button {
                                variant: ButtonVariant::Ghost,
                                disabled: pending,
                                onclick: move |_| confirm_delete.set(false),
                                "Cancel"
                            }
                        }
                    } else {
                        Button {
                            variant: ButtonVariant::Destructive,
                            disabled: pending,
                            onclick: move |_| confirm_delete.set(true),
                            "Delete from catalog"
                        }
                    }
                }
                div {
                    class: "flex gap-2",
                    Button {
                        variant: ButtonVariant::Ghost,
                        disabled: pending,
                        onclick: move |_| on_close.call(()),
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        disabled: pending || selected() == original,
                        onclick: move |_| membership_action.call(selected()),
                        if membership_action.pending() { "Saving..." } else { "Save memberships" }
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

fn create_catalog_command(request: CreateCatalogResource) -> anyhow::Result<GMCommand> {
    let command = match request.kind {
        CatalogCreateKind::Scene => GMCommand::CreateScene {
            scene: SceneCreation {
                name: request.name,
                background_image_url: request.background_image_url,
                background_image_offset: None,
                background_image_scale: (1.0, 1.0),
            },
        },
        CatalogCreateKind::Creature => GMCommand::CreateCreature {
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
            name: request.name,
            content: String::new(),
            visibility: NoteVisibility::GMOnly,
        },
        CatalogCreateKind::Class => GMCommand::CreateClass {
            class: ClassCreation {
                name: request.name,
                abilities: Vec::new(),
                conditions: Vec::new(),
                color: "white".to_string(),
                emoji: None,
            },
        },
        CatalogCreateKind::Ability => GMCommand::CreateAbility {
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
        CatalogCreateKind::Item => GMCommand::CreateItem { name: request.name },
    };
    Ok(command)
}

fn catalog_view_title(game: &Game, view: &CatalogView) -> String {
    match view {
        CatalogView::CurrentScene => "Current scene".to_string(),
        CatalogView::Recent => "Recent".to_string(),
        CatalogView::Type(kind) => kind.plural().to_string(),
        CatalogView::Collections => "Collections".to_string(),
        CatalogView::Collection(id) => game
            .collections
            .get(id)
            .map(|collection| collection.name.clone())
            .unwrap_or_else(|| "Collection".to_string()),
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
            .map(|scene| catalog_scene_entry(game, scene))
            .into_iter()
            .collect(),
        CatalogView::Recent => recent_scene_ids
            .iter()
            .filter_map(|id| game.scenes.get(id))
            .map(|scene| catalog_scene_entry(game, scene))
            .collect(),
        CatalogView::Type(kind) => all_catalog_entries(game)
            .into_iter()
            .filter(|entry| entry.kind == *kind)
            .collect(),
        CatalogView::Collection(id) => collection_entries(game, id),
        CatalogView::Collections => Vec::new(),
    }
}

fn all_catalog_entries(game: &Game) -> Vec<CatalogEntry> {
    let mut entries = Vec::new();

    entries.extend(
        game.scenes
            .values()
            .map(|scene| catalog_scene_entry(game, scene)),
    );
    entries.extend(game.creatures.values().map(|creature| CatalogEntry {
        key: format!("creature:{}", creature.id),
        resource: ResourceRef::Creature(creature.id),
        kind: CatalogResourceKind::Creature,
        name: creature.name.clone(),
        detail: collection_detail(game, ResourceRef::Creature(creature.id)),
        scene_id: None,
        icon_url: (!creature.icon_url.is_empty()).then(|| creature.icon_url.clone()),
        emoji: None,
    }));
    entries.extend(game.classes.values().map(|class| CatalogEntry {
        key: format!("class:{}", class.id),
        resource: ResourceRef::Class(class.id),
        kind: CatalogResourceKind::Class,
        name: class.name.clone(),
        detail: collection_detail(game, ResourceRef::Class(class.id)),
        scene_id: None,
        icon_url: None,
        emoji: Some(class.emoji.clone().unwrap_or_else(|| "🧑‍🎓".to_string())),
    }));
    entries.extend(game.abilities.values().map(|ability| CatalogEntry {
        key: format!("ability:{}", ability.id),
        resource: ResourceRef::Ability(ability.id),
        kind: CatalogResourceKind::Ability,
        name: ability.name.clone(),
        detail: collection_detail(game, ResourceRef::Ability(ability.id)),
        scene_id: None,
        icon_url: None,
        emoji: None,
    }));
    entries.extend(game.items.values().map(|item| CatalogEntry {
        key: format!("item:{}", item.id),
        resource: ResourceRef::Item(item.id),
        kind: CatalogResourceKind::Item,
        name: item.name.clone(),
        detail: collection_detail(game, ResourceRef::Item(item.id)),
        scene_id: None,
        icon_url: None,
        emoji: None,
    }));

    entries.extend(game.notes.values().map(|note| CatalogEntry {
        key: format!("note:{}", note.id),
        resource: ResourceRef::Note(note.id),
        kind: CatalogResourceKind::Note,
        name: note.name.clone(),
        detail: collection_detail(game, ResourceRef::Note(note.id)),
        scene_id: None,
        icon_url: None,
        emoji: None,
    }));

    sort_catalog_entries(&mut entries);
    entries
}

fn collection_entries(game: &Game, id: &CollectionID) -> Vec<CatalogEntry> {
    let Some(collection) = game.collections.get(id) else {
        return Vec::new();
    };
    let detail = Some(collection.name.clone());
    let mut entries = Vec::new();

    entries.extend(collection.scenes.iter().filter_map(|id| {
        game.scenes
            .get(id)
            .map(|scene| scene_entry(scene, detail.clone()))
    }));
    entries.extend(collection.creatures.iter().filter_map(|id| {
        game.creatures.get(id).map(|creature| CatalogEntry {
            key: format!("creature:{}", creature.id),
            resource: ResourceRef::Creature(creature.id),
            kind: CatalogResourceKind::Creature,
            name: creature.name.clone(),
            detail: detail.clone(),
            scene_id: None,
            icon_url: (!creature.icon_url.is_empty()).then(|| creature.icon_url.clone()),
            emoji: None,
        })
    }));
    entries.extend(collection.notes.iter().filter_map(|id| {
        game.notes.get(id).map(|note| CatalogEntry {
            key: format!("note:{}", note.id),
            resource: ResourceRef::Note(note.id),
            kind: CatalogResourceKind::Note,
            name: note.name.clone(),
            detail: detail.clone(),
            scene_id: None,
            icon_url: None,
            emoji: None,
        })
    }));
    entries.extend(collection.classes.iter().filter_map(|id| {
        game.classes.get(id).map(|class| CatalogEntry {
            key: format!("class:{}", class.id),
            resource: ResourceRef::Class(class.id),
            kind: CatalogResourceKind::Class,
            name: class.name.clone(),
            detail: detail.clone(),
            scene_id: None,
            icon_url: None,
            emoji: Some(class.emoji.clone().unwrap_or_else(|| "🧑‍🎓".to_string())),
        })
    }));
    entries.extend(collection.abilities.iter().filter_map(|id| {
        game.abilities.get(id).map(|ability| CatalogEntry {
            key: format!("ability:{}", ability.id),
            resource: ResourceRef::Ability(ability.id),
            kind: CatalogResourceKind::Ability,
            name: ability.name.clone(),
            detail: detail.clone(),
            scene_id: None,
            icon_url: None,
            emoji: None,
        })
    }));
    entries.extend(collection.items.iter().filter_map(|id| {
        game.items.get(id).map(|item| CatalogEntry {
            key: format!("item:{}", item.id),
            resource: ResourceRef::Item(item.id),
            kind: CatalogResourceKind::Item,
            name: item.name.clone(),
            detail: detail.clone(),
            scene_id: None,
            icon_url: None,
            emoji: None,
        })
    }));

    sort_catalog_entries(&mut entries);
    entries
}

fn catalog_scene_entry(game: &Game, scene: &arptypes::Scene) -> CatalogEntry {
    scene_entry(scene, collection_detail(game, ResourceRef::Scene(scene.id)))
}

fn scene_entry(scene: &arptypes::Scene, detail: Option<String>) -> CatalogEntry {
    CatalogEntry {
        key: format!("scene:{}", scene.id),
        resource: ResourceRef::Scene(scene.id),
        kind: CatalogResourceKind::Scene,
        name: scene.name.clone(),
        detail,
        scene_id: Some(scene.id),
        icon_url: None,
        emoji: None,
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

fn all_collections(game: &Game) -> Vec<Collection> {
    let mut collections: Vec<_> = game.collections.values().cloned().collect();
    collections.sort_by(|left, right| {
        left.name
            .to_lowercase()
            .cmp(&right.name.to_lowercase())
            .then_with(|| left.id.cmp(&right.id))
    });
    collections
}

fn collection_contains_resource(collection: &Collection, resource: ResourceRef) -> bool {
    match resource {
        ResourceRef::Scene(id) => collection.scenes.contains(&id),
        ResourceRef::Creature(id) => collection.creatures.contains(&id),
        ResourceRef::Note(id) => collection.notes.contains(&id),
        ResourceRef::Item(id) => collection.items.contains(&id),
        ResourceRef::Ability(id) => collection.abilities.contains(&id),
        ResourceRef::Class(id) => collection.classes.contains(&id),
    }
}

fn collection_resources(resource: ResourceRef) -> CollectionResources {
    match resource {
        ResourceRef::Scene(id) => CollectionResources {
            scenes: vec![id],
            ..Default::default()
        },
        ResourceRef::Creature(id) => CollectionResources {
            creatures: vec![id],
            ..Default::default()
        },
        ResourceRef::Note(id) => CollectionResources {
            notes: vec![id],
            ..Default::default()
        },
        ResourceRef::Item(id) => CollectionResources {
            items: vec![id],
            ..Default::default()
        },
        ResourceRef::Ability(id) => CollectionResources {
            abilities: vec![id],
            ..Default::default()
        },
        ResourceRef::Class(id) => CollectionResources {
            classes: vec![id],
            ..Default::default()
        },
    }
}

fn resource_name(game: &Game, resource: ResourceRef) -> Option<String> {
    match resource {
        ResourceRef::Scene(id) => game.scenes.get(&id).map(|value| value.name.clone()),
        ResourceRef::Creature(id) => game.creatures.get(&id).map(|value| value.name.clone()),
        ResourceRef::Note(id) => game.notes.get(&id).map(|value| value.name.clone()),
        ResourceRef::Item(id) => game.items.get(&id).map(|value| value.name.clone()),
        ResourceRef::Ability(id) => game.abilities.get(&id).map(|value| value.name.clone()),
        ResourceRef::Class(id) => game.classes.get(&id).map(|value| value.name.clone()),
    }
}

fn collection_detail(game: &Game, resource: ResourceRef) -> Option<String> {
    let mut collections: Vec<_> = game
        .collections
        .values()
        .filter(|collection| collection_contains_resource(collection, resource))
        .collect();
    collections.sort_by(|left, right| {
        left.name
            .to_lowercase()
            .cmp(&right.name.to_lowercase())
            .then_with(|| left.id.cmp(&right.id))
    });
    (!collections.is_empty()).then(|| {
        collections
            .into_iter()
            .map(|collection| collection.name.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    })
}

fn collection_resource_count(collection: &Collection) -> usize {
    collection.scenes.len()
        + collection.creatures.len()
        + collection.notes.len()
        + collection.items.len()
        + collection.abilities.len()
        + collection.classes.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collection_detail_covers_every_resource_type_and_sorts_names() {
        let scene_id = arptypes::SceneID::r#gen();
        let creature_id = arptypes::CreatureID::r#gen();
        let note_id = arptypes::NoteID::r#gen();
        let item_id = arptypes::ItemID::r#gen();
        let ability_id = arptypes::AbilityID::r#gen();
        let class_id = arptypes::ClassID::r#gen();
        let mut game = Game::default();

        for name in ["Zeta", "Alpha"] {
            game.collections.insert(Collection {
                id: CollectionID::r#gen(),
                name: name.to_string(),
                scenes: vec![scene_id],
                creatures: vec![creature_id],
                notes: vec![note_id],
                items: vec![item_id],
                abilities: vec![ability_id],
                classes: vec![class_id],
            });
        }

        for resource in [
            ResourceRef::Scene(scene_id),
            ResourceRef::Creature(creature_id),
            ResourceRef::Note(note_id),
            ResourceRef::Item(item_id),
            ResourceRef::Ability(ability_id),
            ResourceRef::Class(class_id),
        ] {
            assert_eq!(
                collection_detail(&game, resource).as_deref(),
                Some("Alpha, Zeta")
            );
        }
    }

    #[test]
    fn search_matches_name_and_resource_type() {
        let entry = CatalogEntry {
            key: "scene:1".to_string(),
            resource: ResourceRef::Scene(arptypes::SceneID::r#gen()),
            kind: CatalogResourceKind::Scene,
            name: "Moonlit Harbor".to_string(),
            detail: None,
            scene_id: None,
            icon_url: None,
            emoji: None,
        };

        assert!(catalog_entry_matches(&entry, "moonlit"));
        assert!(catalog_entry_matches(&entry, "scene"));
        assert!(catalog_entry_matches(&entry, "scenes"));
        assert!(!catalog_entry_matches(&entry, "creature"));
    }
}
