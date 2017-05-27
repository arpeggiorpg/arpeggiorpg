import * as React from "react";
import * as ReactDOM from "react-dom";

interface TextInputProps {
  defaultValue: string;
  styles: object;
  onSubmit: (input: string) => void;
  onCancel: (input: string) => void;
  numbersOnly: boolean;
}

class TextInput extends React.Component<TextInputProps, { value: string }> {

  private input: HTMLInputElement;

  constructor(props: TextInputProps) {
    super(props);
    this.state = { value: props.defaultValue };
  }
  cancel(): void {
    this.props.onCancel(this.state.value);
  }

  handleKeyDown(this: TextInput, event: React.KeyboardEvent<HTMLInputElement>): void {
    if (event.keyCode == 13) {
      if ((this.props.numbersOnly && this.state.value != "-") || !this.props.numbersOnly) {
        this.props.onSubmit(this.state.value);
      }
    } else if (event.keyCode == 27) {
      this.cancel();
    }
  }

  handleInput(event: React.FormEvent<HTMLInputElement>) {
    let value = (event.target as HTMLInputElement).value;
    console.log("handling input", this.props.numbersOnly, value);
    if (this.props.numbersOnly) {
      console.log("Numbers Only!");
      if (!isNaN(Number(value))) {
        console.log("It parsed!", Number(value));
        this.setState({ value: value });
      } else if (value == "-") {
        this.setState({ value: value });
      } else {
        console.log("Not a number!");
      }
    } else {
      this.setState({ value: value })
    }
  }

  componentDidMount() {
    this.input.select();
  }

  render() {
    return <input type="text"
      autoFocus
      ref={(input) => this.input = input}
      value={this.state.value}
      onChange={e => this.handleInput(e)}
      onKeyDown={e => this.handleKeyDown(e)}
      onBlur={e => this.cancel()}
      style={this.props.styles}
    ></input>;
  }
}

export function renderTextInput(app: any, [id, defaultValue, styles, numbersOnly]: [string, string, object, boolean]) {
  let onSubmit = (content: string) => {
    app.ports.textInputSubmit.send([id, content]);
  };
  let onCancel = (content: string) => {
    app.ports.textInputCancel.send([id, content]);
  };
  ReactDOM.render(
    <TextInput defaultValue={defaultValue} styles={styles} onSubmit={onSubmit} onCancel={onCancel}
      numbersOnly={numbersOnly} />,
    document.getElementById(id)
  );
}
