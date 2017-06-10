import * as T from './PTTypes';

export class PTUI {
  public app: T.App;
  elm_app: any;

  constructor(elm_app: any, app: T.App) {
    this.app = app;
    this.elm_app = elm_app;
  }
  public sendCommand(cmd: T.GameCommand) {
    console.log("[sendCommand:TS]", cmd);
    let json = T.encodeGameCommand(cmd);
    this.elm_app.ports.sendCommand.send(json);
  }
}
