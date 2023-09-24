import * as React from "react";

interface TextInputProps {
  defaultValue: string;
  style?: object;
  onSubmit: (input: string) => void;
  onCancel: (input: string) => void;
  numbersOnly?: boolean;
}

export class TextInput extends React.Component<TextInputProps, { value: string }> {

  private input: HTMLInputElement | undefined;

  constructor(props: TextInputProps) {
    super(props);
    this.state = { value: props.defaultValue };
  }

  cancel(): void { this.props.onCancel(this.state.value); }

  handleKeyDown(this: TextInput, event: React.KeyboardEvent<HTMLInputElement>): void {
    if (event.keyCode === 13) {
      if ((this.props.numbersOnly && this.state.value !== "-") || !this.props.numbersOnly) {
        this.props.onSubmit(this.state.value);
      }
    } else if (event.keyCode === 27) {
      this.cancel();
    }
  }

  handleInput(event: React.FormEvent<HTMLInputElement>) {
    const value = event.currentTarget.value;
    if (this.props.numbersOnly) {
      if (!isNaN(Number(value))) {
        this.setState({ value });
      } else if (value === "-") {
        this.setState({ value });
      } else {
        console.log("Not a number!", value);
      }
    } else {
      this.setState({ value });
    }
  }

  componentDidMount() { if (this.input !== undefined) { this.input.select(); } }

  render() {
    return <input type="text"
      autoFocus={true}
      ref={input => { if (input !== null) { this.input = input; } }}
      value={this.state.value}
      onChange={e => this.handleInput(e)}
      onKeyDown={e => this.handleKeyDown(e)}
      onBlur={() => this.cancel()}
      style={this.props.style}
    />;
  }
}
