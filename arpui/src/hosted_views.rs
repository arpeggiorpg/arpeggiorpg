use arptypes::{
    hosted::{HostedGameRequest, InvitationCheck, InvitationID},
    protocol::GameID,
};
use dioxus::prelude::*;
use tracing::error;

use crate::{
    components::button::{Button, ButtonVariant},
    hosted_app::Route,
    hosted_rpi,
    rpi::{send_request, use_ws},
};

#[component]
pub fn Invitations(game_id: GameID) -> Element {
    let ws = use_ws();
    let mut invitations: Signal<Option<Vec<InvitationID>>> = use_signal(|| None);
    let mut load_error: Signal<Option<String>> = use_signal(|| None);

    let _loader: Resource<()> = use_resource(move || async move {
        match send_request::<Vec<InvitationID>>(HostedGameRequest::GMListInvitations, ws).await {
            Ok(list) => invitations.set(Some(list)),
            Err(error) => {
                error!(?error, "Failed to load invitations");
                load_error.set(Some(error.to_string()));
            }
        }
    });

    let mut generate_action = use_action(move |_: ()| async move {
        let invitation_id =
            send_request::<InvitationID>(HostedGameRequest::GMGenerateInvitation, ws).await?;
        let mut current = invitations().unwrap_or_default();
        current.push(invitation_id);
        invitations.set(Some(current));
        Ok::<(), anyhow::Error>(())
    });

    let mut delete_action = use_action(move |invitation_id: InvitationID| async move {
        send_request::<serde_json::Value>(
            HostedGameRequest::GMDeleteInvitation { invitation_id },
            ws,
        )
        .await?;
        invitations.set(Some(
            invitations()
                .unwrap_or_default()
                .into_iter()
                .filter(|id| *id != invitation_id)
                .collect(),
        ));
        Ok::<(), anyhow::Error>(())
    });

    let base_url = web_sys::window()
        .and_then(|window| window.location().origin().ok())
        .unwrap_or_default();

    rsx! {
        div {
            class: "bg-white rounded-lg shadow-md p-4",
            div {
                class: "flex items-center justify-between mb-3",
                h2 { class: "text-lg font-semibold text-gray-800", "Invitations" }
                Button {
                    variant: ButtonVariant::Primary,
                    disabled: generate_action.pending(),
                    onclick: move |_| generate_action.call(()),
                    if generate_action.pending() { "Generating..." } else { "Generate New Link" }
                }
            }
            if let Some(error) = load_error() {
                p { class: "text-red-600 text-sm", "Failed to load invitations: {error}" }
            }
            match invitations() {
                None => rsx! { p { class: "text-sm text-gray-500", "Loading invitations..." } },
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

#[component]
pub fn AcceptInvitationPage(game_id: GameID, invitation_id: InvitationID) -> Element {
    let mut profile_name = use_signal(String::new);
    let mut accept_error: Signal<Option<String>> = use_signal(|| None);
    let navigator = navigator();
    let check =
        use_resource(
            move || async move { hosted_rpi::check_invitation(game_id, invitation_id).await },
        );

    let mut accept_action = use_action(move |name: String| {
        let navigator = navigator.clone();
        async move {
            match hosted_rpi::accept_invitation(game_id, invitation_id, name).await {
                Ok(()) => {
                    navigator.push(Route::GameListPage);
                }
                Err(error) => {
                    error!(?error, "Failed to accept invitation");
                    accept_error.set(Some("Something went wrong. Please try again.".to_string()));
                }
            }
            Ok::<(), anyhow::Error>(())
        }
    });

    let mut do_accept = move || {
        let name = profile_name().trim().to_string();
        if !name.is_empty() {
            accept_error.set(None);
            accept_action.call(name);
        }
    };

    match &*check.read() {
        None => rsx! {
            div { class: "flex h-full items-center justify-center",
                p { class: "text-gray-500", "Checking invitation..." }
            }
        },
        Some(Err(error)) => rsx! {
            div { class: "flex h-full items-center justify-center",
                p { class: "text-red-600", "Error checking invitation: {error}" }
            }
        },
        Some(Ok(InvitationCheck {
            invitation_valid: false,
            ..
        })) => rsx! {
            div { class: "flex h-full items-center justify-center",
                div { class: "bg-white rounded-lg shadow-md p-6 text-center",
                    p { class: "text-gray-700", "Sorry, that invitation doesn't seem to exist." }
                    Link {
                        to: Route::GameListPage {},
                        class: "text-blue-700 text-sm mt-4 inline-block",
                        "Back to Game List"
                    }
                }
            }
        },
        Some(Ok(
            check @ InvitationCheck {
                already_member: true,
                ..
            },
        )) => {
            let game_name = check
                .game_name
                .clone()
                .unwrap_or_else(|| "this game".to_string());
            let game_link =
                check
                    .member_profile_name
                    .as_ref()
                    .map(|player_id| Route::PlayerGamePage {
                        id: game_id,
                        player_id: player_id.clone(),
                    });
            rsx! {
                div { class: "flex h-full items-center justify-center",
                    div { class: "bg-white rounded-lg shadow-md p-6 flex flex-col gap-4 text-center",
                        h2 { class: "text-lg font-semibold text-gray-800", "Already a member" }
                        p { class: "text-sm text-gray-600", "You're already a member of {game_name}." }
                        if let Some(route) = game_link {
                            Link { to: route, class: "text-blue-700 text-sm", "Go to {game_name} →" }
                        } else {
                            Link {
                                to: Route::GameListPage {},
                                class: "text-blue-700 text-sm",
                                "Go to Game List"
                            }
                        }
                    }
                }
            }
        }
        Some(Ok(
            check @ InvitationCheck {
                invitation_valid: true,
                already_member: false,
                ..
            },
        )) => {
            let game_name = check
                .game_name
                .clone()
                .unwrap_or_else(|| "a game".to_string());
            rsx! {
                div { class: "flex h-full items-center justify-center",
                    div { class: "bg-white rounded-lg shadow-md p-6 flex flex-col gap-4",
                        h2 {
                            class: "text-lg font-semibold text-gray-800",
                            "You've been invited to {game_name}!"
                        }
                        p { class: "text-sm text-gray-600", "Enter a name to join as a player." }
                        input {
                            class: "border rounded px-3 py-2 w-full",
                            r#type: "text",
                            placeholder: "Your player name",
                            value: "{profile_name}",
                            autofocus: true,
                            oninput: move |event| profile_name.set(event.value()),
                            onkeydown: move |event| {
                                if event.key() == Key::Enter && !accept_action.pending() {
                                    do_accept();
                                }
                            },
                        }
                        if let Some(error) = accept_error() {
                            p { class: "text-red-600 text-sm", "{error}" }
                        }
                        div { class: "flex justify-end gap-2",
                            Link {
                                to: Route::GameListPage {},
                                class: "text-sm text-gray-500 py-2 px-3",
                                "Cancel"
                            }
                            Button {
                                variant: ButtonVariant::Primary,
                                disabled: accept_action.pending() || profile_name().trim().is_empty(),
                                onclick: move |_| do_accept(),
                                if accept_action.pending() { "Joining..." } else { "Join as a Player" }
                            }
                        }
                    }
                }
            }
        }
    }
}
