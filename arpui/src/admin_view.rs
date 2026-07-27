use std::collections::{HashMap, HashSet};

use arptypes::multitenant::{CopyToPreprodResult, GameMetadata};
use dioxus::prelude::*;
use serde::Deserialize;
use tracing::info;

use crate::{
    components::{
        button::{Button, ButtonVariant},
        modal::Modal,
    },
    rpi::{copy_game_from_production, current_user, delete_preprod_copy, rpi_get},
};

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DurableObject {
    id: String,
    has_stored_data: Option<bool>,
}

#[derive(Clone, Debug, Deserialize)]
struct CloudflareApiResponse {
    result: Option<Vec<DurableObject>>,
}

#[derive(Clone, Debug, Deserialize)]
struct Namespace {
    id: String,
    name: Option<String>,
    _class: Option<String>,
}

#[derive(Clone, Debug, Deserialize)]
struct NamespacesResponse {
    result: Option<Vec<Namespace>>,
}

#[derive(Clone, Debug, Deserialize)]
struct SuperuserGamesResponse {
    games: Vec<(String, GameMetadata)>,
    preprod_games: Option<Vec<(String, GameMetadata)>>,
    do_namespaces: NamespacesResponse,
    do_objects: HashMap<String, CloudflareApiResponse>,
    arpeggiogame_ids: HashMap<String, String>,
}

#[derive(Clone, Debug)]
struct OrphanDo {
    id: String,
    namespace: String,
    has_stored_data: Option<bool>,
}

#[derive(Clone, Copy)]
struct DoStatus {
    present: bool,
    has_data: bool,
}

#[component]
pub fn AdminPage() -> Element {
    let me: Resource<anyhow::Result<crate::rpi::CurrentUser>> =
        use_resource(move || async move { current_user().await });

    match &*me.read() {
        Some(Ok(me)) if me.is_superuser => rsx! { SuperuserAdminPage {} },
        Some(Ok(_)) => rsx! {
            div {
                class: "p-6",
                div {
                    class: "rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-800",
                    "You are not authorized to view this page."
                }
            }
        },
        Some(Err(err)) => rsx! {
            div {
                class: "p-6",
                div {
                    class: "rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-800",
                    "Error loading user permissions: {err}"
                }
            }
        },
        None => rsx! {
            div {
                class: "p-6 text-sm text-gray-500",
                "Checking permissions..."
            }
        },
    }
}

#[component]
fn SuperuserAdminPage() -> Element {
    let is_preprod = crate::rpi::is_preprod();
    let mut reload_nonce = use_signal(|| 0u32);
    let mut status_message = use_signal(|| None::<String>);
    let mut error_message = use_signal(|| None::<String>);
    let mut last_dump = use_signal(|| None::<String>);
    let mut destroy_target = use_signal(|| None::<(String, String)>);
    let mut copying_game_id = use_signal(|| None::<String>);
    let mut copy_result = use_signal(|| None::<(String, CopyToPreprodResult)>);
    let mut copy_action = use_action(move |(game_id, game_name): (String, String)| async move {
        copying_game_id.set(Some(game_id.clone()));
        copy_result.set(None);
        error_message.set(None);
        status_message.set(Some(format!("Copying {game_name} from production...")));

        match copy_game_from_production(&game_id).await {
            Ok(result) => {
                status_message.set(Some(format!("Copied {game_name} from production.")));
                copy_result.set(Some((game_name, result)));
                reload_nonce.set(reload_nonce() + 1);
            }
            Err(err) => {
                status_message.set(None);
                error_message.set(Some(format!(
                    "Copy from production failed for {game_name}: {err}"
                )));
            }
        }

        copying_game_id.set(None);
        Ok::<(), anyhow::Error>(())
    });

    let data: Resource<anyhow::Result<SuperuserGamesResponse>> = use_resource(move || async move {
        let _ = reload_nonce();
        rpi_get("superuser/games").await
    });

    match &*data.read() {
        Some(Ok(data)) => {
            let orphan_dos = if is_preprod {
                Vec::new()
            } else {
                find_orphan_dos(data)
            };
            rsx! {
                div {
                    class: "p-6 space-y-6",
                    div {
                        class: "rounded-xl border border-gray-200 bg-white p-5 shadow-sm",
                        h1 {
                            class: "text-2xl font-semibold text-gray-900",
                            "Admin"
                        }
                        p {
                            class: "mt-1 text-sm text-gray-600",
                            "Superuser dashboard for game metadata and durable object health."
                        }
                    }

                    if let Some(msg) = status_message() {
                        div {
                            class: "rounded-lg border border-blue-200 bg-blue-50 px-4 py-3 text-sm text-blue-800",
                            "{msg}"
                        }
                    }
                    if let Some(msg) = error_message() {
                        div {
                            class: "rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-800",
                            "{msg}"
                        }
                    }
                    if let Some((game_name, result)) = copy_result() {
                        div {
                            class: "rounded-lg border border-green-200 bg-green-50 px-4 py-3 text-sm text-green-900",
                            div {
                                class: "font-medium",
                                "Preprod copy ready: {game_name}"
                            }
                            div {
                                class: "mt-1",
                                "Storage version: {result.storage_version}"
                            }
                            a {
                                class: "mt-2 inline-block font-medium text-blue-700 underline",
                                href: "{result.game_url}",
                                target: "_blank",
                                rel: "noopener noreferrer",
                                "Open preprod game"
                            }
                        }
                    }

                    div {
                        class: "rounded-xl border border-gray-200 bg-white shadow-sm overflow-hidden",
                        div {
                            class: "border-b border-gray-200 px-4 py-3",
                            h2 {
                                class: "font-medium text-gray-900",
                                if is_preprod {
                                    "Production games"
                                } else {
                                    "All Games in game_metadata"
                                }
                            }
                        }
                        div {
                            class: "overflow-x-auto",
                            table {
                                class: "min-w-full text-sm",
                                thead {
                                    class: "bg-gray-50 text-gray-600",
                                    tr {
                                        th { class: "px-4 py-3 text-left font-medium", "Game ID" }
                                        th { class: "px-4 py-3 text-left font-medium", "Name" }
                                        th {
                                            class: "px-4 py-3 text-center font-medium",
                                            if is_preprod {
                                                "Preprod DO"
                                            } else {
                                                "Game DO"
                                            }
                                        }
                                        th { class: "px-4 py-3 text-center font-medium", "Actions" }
                                    }
                                }
                                tbody {
                                    class: "divide-y divide-gray-100",
                                    for (game_id, meta) in data.games.iter() {
                                        {
                                            let game_id = game_id.clone();
                                            let game_name = meta.name.clone();
                                            let do_status = get_do_status(&game_id, data);
                                            let is_registered_in_preprod =
                                                is_preprod_game_registered(&game_id, data);
                                            let is_present = if is_preprod {
                                                is_registered_in_preprod
                                            } else {
                                                do_status.present
                                            };
                                            let is_available = if is_preprod {
                                                is_registered_in_preprod
                                            } else {
                                                do_status.has_data
                                            };
                                            let game_id_for_dump = game_id.clone();
                                            let game_name_for_dump = game_name.clone();
                                            let game_id_for_destroy = game_id.clone();
                                            let game_name_for_destroy = game_name.clone();
                                            let game_id_for_copy = game_id.clone();
                                            let game_name_for_copy = game_name.clone();
                                            let production_game_url =
                                                format!("https://arpeggiogame.com/gm/{game_id}");
                                            let preprod_game_url = format!("/gm/{game_id}");
                                            let is_copying = copy_action.pending()
                                                && copying_game_id().as_deref() == Some(game_id.as_str());
                                            rsx! {
                                                tr {
                                                    key: "{game_id}",
                                                    td {
                                                        class: "px-4 py-3 font-mono text-xs text-gray-700",
                                                        "{game_id}"
                                                    }
                                                    td {
                                                        class: "px-4 py-3",
                                                        div {
                                                            class: "font-medium text-gray-900",
                                                            "{game_name}"
                                                        }
                                                        div {
                                                            class: "mt-1 flex items-center gap-3 text-xs",
                                                            a {
                                                                class: "font-medium text-blue-700 underline",
                                                                href: "{production_game_url}",
                                                                target: "_blank",
                                                                rel: "noopener noreferrer",
                                                                "Prod"
                                                            }
                                                            if is_preprod && is_registered_in_preprod {
                                                                a {
                                                                    class: "font-medium text-blue-700 underline",
                                                                    href: "{preprod_game_url}",
                                                                    target: "_blank",
                                                                    rel: "noopener noreferrer",
                                                                    "Preprod"
                                                                }
                                                            }
                                                        }
                                                    }
                                                    td {
                                                        class: "px-4 py-3 text-center",
                                                        StatusPill {
                                                            present: is_present,
                                                            has_data: do_status.has_data,
                                                        }
                                                    }
                                                    td {
                                                        class: "px-4 py-3",
                                                        div {
                                                            class: "flex items-center justify-center gap-2",
                                                            if is_preprod && !is_registered_in_preprod {
                                                                Button {
                                                                    variant: ButtonVariant::Outline,
                                                                    disabled: copy_action.pending(),
                                                                    onclick: move |_| {
                                                                        copy_action.call((
                                                                            game_id_for_copy.clone(),
                                                                            game_name_for_copy.clone(),
                                                                        ));
                                                                    },
                                                                    if is_copying {
                                                                        "Copying..."
                                                                    } else {
                                                                        "Copy from production"
                                                                    }
                                                                }
                                                            }
                                                            if is_available {
                                                                Button {
                                                                    variant: ButtonVariant::Ghost,
                                                                    onclick: move |_| {
                                                                        let game_id = game_id_for_dump.clone();
                                                                        let game_name = game_name_for_dump.clone();
                                                                        async move {
                                                                            status_message.set(Some(format!("Dumping {game_name}...")));
                                                                            error_message.set(None);
                                                                            match rpi_get::<serde_json::Value>(&format!("superuser/dump/{game_id}")).await {
                                                                                Ok(value) => {
                                                                                    let pretty = serde_json::to_string_pretty(&value)
                                                                                        .unwrap_or_else(|_| value.to_string());
                                                                                    info!(game_id, game_name, dump=?value, "superuser dump");
                                                                                    status_message.set(Some(format!("Dumped {game_name}. Output is shown below and logged.")));
                                                                                    last_dump.set(Some(pretty));
                                                                                }
                                                                                Err(err) => {
                                                                                    error_message.set(Some(format!("Dump failed for {game_name}: {err}")));
                                                                                }
                                                                            }
                                                                        }
                                                                    },
                                                                    "Dump"
                                                                }
                                                                Button {
                                                                    variant: ButtonVariant::Ghost,
                                                                    class: "text-red-700 hover:text-red-800".to_string(),
                                                                onclick: move |_| {
                                                                    destroy_target.set(Some((game_id_for_destroy.clone(), game_name_for_destroy.clone())));
                                                                },
                                                                    if is_preprod {
                                                                        "Delete copy"
                                                                    } else {
                                                                        "Destroy"
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
                            }
                        }
                    }

                    if !orphan_dos.is_empty() {
                        div {
                            class: "rounded-xl border border-gray-200 bg-white shadow-sm overflow-hidden",
                            div {
                                class: "border-b border-gray-200 px-4 py-3",
                                h2 { class: "font-medium text-gray-900", "Orphan Durable Objects" }
                                p {
                                    class: "mt-1 text-sm text-gray-600",
                                    "These DOs exist in Cloudflare but are not mapped to any known game."
                                }
                            }
                            div {
                                class: "overflow-x-auto",
                                table {
                                    class: "min-w-full text-sm",
                                    thead {
                                        class: "bg-gray-50 text-gray-600",
                                        tr {
                                            th { class: "px-4 py-3 text-left font-medium", "DO ID" }
                                            th { class: "px-4 py-3 text-left font-medium", "Namespace" }
                                            th { class: "px-4 py-3 text-center font-medium", "Has Data" }
                                        }
                                    }
                                    tbody {
                                        class: "divide-y divide-gray-100",
                                        for orphan in orphan_dos {
                                            tr {
                                                key: "{orphan.namespace}:{orphan.id}",
                                                td {
                                                    class: "px-4 py-3 font-mono text-xs text-red-700",
                                                    "{orphan.id}"
                                                }
                                                td {
                                                    class: "px-4 py-3 font-mono text-xs text-gray-700",
                                                    "{orphan.namespace}"
                                                }
                                                td {
                                                    class: "px-4 py-3 text-center text-gray-800",
                                                    {render_has_data(orphan.has_stored_data)}
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if let Some(dump) = last_dump() {
                        div {
                            class: "rounded-xl border border-gray-200 bg-white shadow-sm overflow-hidden",
                            div {
                                class: "border-b border-gray-200 px-4 py-3",
                                h2 { class: "font-medium text-gray-900", "Latest Dump Output" }
                            }
                            pre {
                                class: "max-h-[30rem] overflow-auto bg-gray-950 text-gray-100 text-xs p-4",
                                "{dump}"
                            }
                        }
                    }

                    Modal {
                        open: destroy_target().is_some(),
                        on_close: move |_| destroy_target.set(None),
                        modal_class: "bg-white rounded-lg shadow-xl max-w-lg w-full mx-auto".to_string(),
                        div {
                            class: "p-5 space-y-4",
                            h3 {
                                class: "text-lg font-semibold text-gray-900",
                                if is_preprod {
                                    "Delete preprod copy"
                                } else {
                                    "Destroy Game"
                                }
                            }
                            if let Some((game_id, game_name)) = destroy_target() {
                                p {
                                    class: "text-sm text-gray-700",
                                    if is_preprod {
                                        "This will delete the preprod copy of "
                                    } else {
                                        "This will permanently destroy "
                                    }
                                    span { class: "font-semibold", "{game_name}" }
                                    " ("
                                    span { class: "font-mono text-xs", "{game_id}" }
                                    ")."
                                }
                                p {
                                    class: if is_preprod {
                                        "text-sm text-gray-700"
                                    } else {
                                        "text-sm text-red-700"
                                    },
                                    if is_preprod {
                                        "Production is not changed. You can copy the game again afterward."
                                    } else {
                                        "This action is irreversible."
                                    }
                                }
                                div {
                                    class: "flex justify-end gap-2",
                                    Button {
                                        variant: ButtonVariant::Ghost,
                                        onclick: move |_| destroy_target.set(None),
                                        "Cancel"
                                    }
                                    Button {
                                        variant: ButtonVariant::Primary,
                                        class: "bg-red-600 hover:bg-red-700".to_string(),
                                        onclick: move |_| {
                                            let game_id = game_id.clone();
                                            let game_name = game_name.clone();
                                            async move {
                                                error_message.set(None);
                                                status_message.set(Some(if is_preprod {
                                                    format!("Deleting the preprod copy of {game_name}...")
                                                } else {
                                                    format!("Destroying {game_name}...")
                                                }));
                                                let result = if is_preprod {
                                                    delete_preprod_copy(&game_id).await
                                                } else {
                                                    rpi_get::<serde_json::Value>(&format!("superuser/destroy/{game_id}"))
                                                        .await
                                                        .map(|_| ())
                                                };
                                                match result {
                                                    Ok(result) => {
                                                        info!(game_id, game_name, destroy=?result, "superuser destroy");
                                                        status_message.set(Some(if is_preprod {
                                                            format!("Deleted the preprod copy of {game_name}.")
                                                        } else {
                                                            format!("Destroyed {game_name}.")
                                                        }));
                                                        destroy_target.set(None);
                                                        last_dump.set(None);
                                                        copy_result.set(None);
                                                        reload_nonce.set(reload_nonce() + 1);
                                                    }
                                                    Err(err) => {
                                                        error_message.set(Some(if is_preprod {
                                                            format!("Deleting the preprod copy failed for {game_name}: {err}")
                                                        } else {
                                                            format!("Destroy failed for {game_name}: {err}")
                                                        }));
                                                    }
                                                }
                                            }
                                        },
                                        if is_preprod {
                                            "Delete copy"
                                        } else {
                                            "Destroy Game"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Some(Err(err)) => rsx! {
            div {
                class: "p-6",
                div {
                    class: "rounded-lg border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-800",
                    "Failed to load admin data: {err}"
                }
            }
        },
        None => rsx! {
            div {
                class: "p-6 text-gray-500",
                "Loading admin dashboard..."
            }
        },
    }
}

#[component]
fn StatusPill(present: bool, has_data: bool) -> Element {
    if !present {
        return rsx! {
            span {
                class: "inline-flex items-center rounded-full border border-red-200 bg-red-50 px-2.5 py-1 text-xs font-medium text-red-700",
                "Missing"
            }
        };
    }

    let icon = if has_data { "💾" } else { "☐" };
    rsx! {
        span {
            class: "inline-flex items-center gap-1 rounded-full border border-green-200 bg-green-50 px-2.5 py-1 text-xs font-medium text-green-700",
            "Present {icon}"
        }
    }
}

fn render_has_data(has_stored_data: Option<bool>) -> String {
    match has_stored_data {
        Some(true) => "💾".to_string(),
        Some(false) => "☐".to_string(),
        None => "?".to_string(),
    }
}

fn get_do_status(game_id: &str, data: &SuperuserGamesResponse) -> DoStatus {
    let do_id = data.arpeggiogame_ids.get(game_id);
    let mut present = false;
    let mut has_data = false;

    for objects_response in data.do_objects.values() {
        let Some(objects) = &objects_response.result else {
            continue;
        };
        for obj in objects {
            if Some(&obj.id) == do_id {
                present = true;
                has_data = obj.has_stored_data.unwrap_or(false);
            }
        }
    }

    DoStatus { present, has_data }
}

fn is_preprod_game_registered(game_id: &str, data: &SuperuserGamesResponse) -> bool {
    data.preprod_games
        .as_ref()
        .is_some_and(|games| games.iter().any(|(id, _)| id == game_id))
}

fn find_orphan_dos(data: &SuperuserGamesResponse) -> Vec<OrphanDo> {
    let known_do_ids: HashSet<String> = data.arpeggiogame_ids.values().cloned().collect();

    let mut namespace_id_to_name: HashMap<String, String> = HashMap::new();
    if let Some(namespaces) = &data.do_namespaces.result {
        for ns in namespaces {
            if let Some(name) = &ns.name {
                namespace_id_to_name.insert(ns.id.clone(), name.clone());
            }
        }
    }

    let mut orphans = Vec::new();
    for (namespace_id, objects_response) in &data.do_objects {
        let Some(objects) = &objects_response.result else {
            continue;
        };
        for obj in objects {
            if known_do_ids.contains(&obj.id) {
                continue;
            }
            let namespace = namespace_id_to_name
                .get(namespace_id)
                .cloned()
                .unwrap_or_else(|| namespace_id.clone());
            orphans.push(OrphanDo {
                id: obj.id.clone(),
                namespace,
                has_stored_data: obj.has_stored_data,
            });
        }
    }

    orphans.sort_by(|a, b| a.namespace.cmp(&b.namespace).then(a.id.cmp(&b.id)));
    orphans
}
