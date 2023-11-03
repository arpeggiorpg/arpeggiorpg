import React from "react";
import { useParams } from "react-router-dom";
import { Modal } from "semantic-ui-react";

import * as M from "./Model";
import * as WS from "./wsrpi";
import * as T from "./PTTypes";

export default function Connector(props: { role: T.Role } & React.PropsWithChildren) {
  const { gameId } = useParams() as { gameId: string };
  const [connectionCount, setConnectionCount] = React.useState(0);

  React.useEffect(() => {
    // connect returns a cancellation function, which we return here from the effect so react will
    // call it when this component gets unmounted.
    return WS.connect(gameId, props.role);
  }, [gameId, connectionCount]);

  const status = M.useState((s) => s.socketStatus);
  const reconnect = () => setConnectionCount(connectionCount + 1);

  return (
    <>
      {status === "closed"
        ? (
          <Modal open={true}>
            <Modal.Header>Connection is inactive</Modal.Header>
            <Modal.Content>
              Connection is inactive (most likely due to idle timeout; I'm just trying to avoid
              running up a big server bill!)
            </Modal.Content>
            <Modal.Actions>
              <button onClick={reconnect}>Reconnect</button>
            </Modal.Actions>
          </Modal>
        )
        : null}
      {props.children}
    </>
  );
}
