import {
  CredentialResponse,
  GoogleLogin,
  GoogleOAuthProvider,
} from "@react-oauth/google";
import * as A from "./Actions";

export function SignIn() {
  return (
    <GoogleOAuthProvider clientId="328154234071-c7una5er0n385sdgvih81ngbkgp1l7nj.apps.googleusercontent.com">
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
      document.cookie = `pt-id-token=${credentialResponse.credential}`;
      A.newGetGame("6f347d4c-0bbe-4381-a7b0-e2df22e08144");
    } else {
      console.error("didn't get credentials???", credentialResponse);
    }
  }
  function onError() {
    console.error("Failed to log in with google!");
  }
}
