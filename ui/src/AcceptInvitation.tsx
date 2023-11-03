import React from "react";
import { useNavigate, useParams } from "react-router-dom";
import useSWR from "swr";
import * as Z from "zod";
import { ptfetch } from "./Actions";

export default function AcceptInvitation() {
    const { gameId, invitationId } = useParams() as {
      gameId: string;
      invitationId: string;
    };
    const navigate = useNavigate();

    const [profileName, setProfileName] = React.useState<string>("");

    const { data: invitationExists, isLoading } = useSWR(
      `/g/invitations/${gameId}/${invitationId}`,
      (k) => ptfetch(k, {}, Z.boolean()),
    );
    if (isLoading) {
      return <div>Checking invitation...</div>;
    }

    if (invitationExists) {
      return (
        <div>
          <p>Invitation checks out!</p>
          <p>Wanna join? Enter a name!</p>
          <input
            type="text"
            value={profileName}
            onChange={(e) => setProfileName(e.target.value)}
          />
          <button onClick={accept}>Join as a player!</button>
        </div>
      );
    } else {
      return <div>Sorry, that invitation doesn't seem to exist.</div>;
    }

    async function accept() {
      await ptfetch(
        `/g/invitations/${gameId}/${invitationId}/accept`,
        {
          method: "POST",
          body: JSON.stringify(profileName),
          headers: { "content-type": "application/json" },
        },
        Z.any(),
      );
      navigate(`/`);
    }
  }
