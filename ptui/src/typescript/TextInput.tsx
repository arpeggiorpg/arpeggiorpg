import * as React from "react";
import * as ReactDOM from "react-dom";

interface TextInputProps {
  defaultValue: string;
  styles: object;
  onSubmit: (input: string) => void;
  onCancel: (input: string) => void;
}

class TextInput extends React.Component<TextInputProps, undefined> {
  /// Exposes events that 
  render() {
    return <input type="text" defaultValue={this.props.defaultValue}></input>;
  }
}

export function renderTextInput(app: any, [id, defaultValue, styles]: [string, string, object]) {
  let onSubmit = (content: string) => {
    app.ports.textInputSubmit.send([id, content]);
  };
  let onCancel = (content: string) => {
    app.ports.textInputCancel.send([id, content]);
  };
  ReactDOM.render(
    <TextInput defaultValue={defaultValue} styles={styles} onSubmit={onSubmit} onCancel={onCancel}/>,
    document.getElementById(id)
  );
}
