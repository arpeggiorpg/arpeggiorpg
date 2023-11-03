import { CredentialResponse, GoogleLogin, GoogleOAuthProvider } from "@react-oauth/google";
import { setCookie } from "react-use-cookie";
import * as A from "./Actions";
import * as M from "./Model";

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
        <GoogleLogin onSuccess={onSignIn} onError={onError} auto_select />
      </div>
    </GoogleOAuthProvider>
  );

  function onSignIn(credentialResponse: CredentialResponse) {
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
