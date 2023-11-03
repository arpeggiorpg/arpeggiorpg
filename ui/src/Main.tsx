import * as React from "react";

import { createBrowserRouter, Link, Outlet } from "react-router-dom";

import * as M from "./Model";
import { SignIn } from "./SignIn";

const GMView = React.lazy(() => import("./GMView"));
const GMMap = React.lazy(() => import("./GMView").then(GMV => ({ default: GMV.GMMap })));
const Admin = React.lazy(() => import("./Admin"));
const GameList = React.lazy(() => import("./GameList"));
const AcceptInvitation = React.lazy(() => import("./AcceptInvitation"));
const PlayerView = React.lazy(() => import("./PlayerView"));

export const router = createBrowserRouter([
  {
    path: "/",
    element: <Main />,
    children: [
      { index: true, element: <GameList /> },
      {
        path: "/invitations/:gameId/:invitationId",
        element: <AcceptInvitation />,
      },
      { path: "player/:gameId", element: <PlayerView /> },
      {
        path: "gm/:gameId",
        element: <GMView />,
        children: [
          { index: true, element: <div>Pick a scene!</div> },
          {
            path: "campaign/*",
            element: (
              <React.Suspense fallback={<div>Loading...</div>}>
                <GMMap />
              </React.Suspense>
            ),
          },
        ],
      },
    ],
  },
  {
    path: "/admin",
    element: <Admin />,
  },
]);

export function Main() {
  const token = M.useState((s) => s.userToken);
  const gameName = M.useState((s) => s.gameName);

  if (token && token.length > 1) {
    return (
      <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
        <div style={{ display: "flex", justifyContent: "space-between" }}>
          <h1>
            <Link to="/">ArpeggioRPG</Link> {gameName ? <span>— {gameName}</span> : null}
          </h1>
          <div className="rightNavThing">
            <Link to="/">Game List</Link>
            <button onClick={() => M.getState().setUserToken(undefined)}>
              Log Off
            </button>
          </div>
        </div>
        <ErrorBoundary fallback={<div>ERROR OCCURRED</div>}>
          <React.Suspense fallback={<div>Loading...</div>}>
            <Outlet />
          </React.Suspense>
        </ErrorBoundary>
      </div>
    );
  } else {
    return <SignIn />;
  }
}

type ErrorBoundaryProps = {
  fallback: React.ReactElement;
} & React.PropsWithChildren;

class ErrorBoundary extends React.Component<
  ErrorBoundaryProps,
  { hasError: boolean }
> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError() {
    // Update state so the next render will show the fallback UI.
    return { hasError: true };
  }

  componentDidCatch(error: Error, info: React.ErrorInfo) {
    console.error("[ErrorBoundary error]", error);
    console.log("Additional Stack:", info.componentStack);
  }

  render() {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return this.props.fallback;
    }

    return this.props.children;
  }
}
