import * as React from "react";
import * as ReactDOM from "react-dom";

interface TextInputProps {
  defaultValue: string;
  styles: object;
  onSubmit: (input: string) => void;
  onCancel: (input: string) => void;
}

class TextInput extends React.Component<TextInputProps, {value: string}> {

  constructor(props: TextInputProps) {
    super(props);
    this.state = { value: props.defaultValue };
  }

  handleKeyDown(this: TextInput, event: React.KeyboardEvent<HTMLInputElement>): void {
    if (event.keyCode == 13) {
      this.props.onSubmit(this.state.value);
    } else if (event.keyCode == 27) {
      this.props.onCancel(this.state.value);
    }
  }

  render() {
    return <input type="text"
                  value={this.state.value}
                  onChange={e => this.setState({value: e.target.value})}
                  onKeyDown={ e => this.handleKeyDown(e)}></input>;
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
