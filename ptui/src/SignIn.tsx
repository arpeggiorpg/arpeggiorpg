import {
  CredentialResponse,
  GoogleLogin,
  GoogleOAuthProvider,
} from "@react-oauth/google";
import * as A from "./Actions";
import * as M from "./Model";
import { setCookie } from "react-use-cookie";

export function SignIn() {
  return (
    <GoogleOAuthProvider clientId={import.meta.env.VITE_GOOGLE_CLIENT_ID}>
      <div
        style={{
          height: "500px",
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
        }}
      >
        <GoogleLogin onSuccess={onSignIn} onError={onError} />
      </div>
    </GoogleOAuthProvider>
  );

  function onSignIn(credentialResponse: CredentialResponse) {
    // 1. all requests to the backend need to have this credential passed down. So, should we put it in a cookie?
    console.log("credential response!", credentialResponse);
    if (credentialResponse.credential) {
      M.getState().setUserToken(credentialResponse.credential);
      setCookie(M.ID_TOKEN_NAME, credentialResponse.credential);
    } else {
      console.error("didn't get credentials???", credentialResponse);
    }
  }
  function onError() {
    console.error("Failed to log in with google!");
  }
}
