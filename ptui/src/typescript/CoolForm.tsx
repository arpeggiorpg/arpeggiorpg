/** Cool Forms
 *
 * This code is a piece of shit. I use `any` all over the place. And it doesn't even expose a
 * typesafe API.
 */

import * as LD from 'lodash';
import * as React from 'react';
import { Button, Form, Input, Message } from 'semantic-ui-react';

interface BaseInputProps { label: string; name: string; style?: any; }

interface CoolTextInputProps extends BaseInputProps {
  default: string; style?: any;
  nonEmpty?: boolean;
}
export class PlaintextInput extends React.Component<CoolTextInputProps> { }

interface CoolNumericProps extends BaseInputProps { default: number; min?: number; }
export class NumericInput extends React.Component<CoolNumericProps> { }

interface CoolSubmitProps {
  onClick: (data: { [index: string]: any }) => void;
  children: React.ReactNode;
  disabled?: boolean;
}
export class Submit extends React.Component<CoolSubmitProps> { }

interface CoolFormState { data: { [index: string]: { type: string, value: any, extra: any } }; }

export class CoolForm extends React.Component<{}, CoolFormState> {
  constructor(props: { children: React.ReactNode }) {
    super(props);
    const data: { [index: string]: { type: string, value: any, extra: any } } = {};
    this.walkChildren(props.children, input => {
      switch (input.type) {
        case PlaintextInput:
          {
            const props = input.props as CoolTextInputProps;
            data[props.name] = {
              type: "text", value: props.default,
              extra: { nonEmpty: props.nonEmpty },
            };
          }
          break;
        case NumericInput:
          {
            const props = input.props as CoolNumericProps;
            data[props.name] = {
              type: "numeric", value: props.default,
              extra: { min: props.min },
            };
          }
          break;
      }
    }, () => undefined);
    this.state = { data };
  }

  walkChildren(
    children: any, handleInput: (el: React.ReactElement<BaseInputProps>) => any,
    handleSubmit: (el: React.ReactElement<CoolSubmitProps>) => any):
    Array<React.ReactNode> {
    return React.Children.map(children, child => {
      if (typeof child === "object" && child.type) {
        switch (child.type) {
          case PlaintextInput:
          case NumericInput:
            return handleInput(child);
          case Submit:
            return handleSubmit(child);
          default:
            if (!(React.isValidElement(child) && (child.props as any).children)) { return child; }
            return React.cloneElement(child as any,
              {
                children: this.walkChildren(
                  (child.props as any).children, handleInput, handleSubmit),
              });
        }
      }
    });
  }

  render(): JSX.Element {
    const errors = this.getErrors();
    return <Form error={true}>{this.walkChildren(this.props.children,
      el => {
        const error = errors[el.props.name];
        return <Form.Field style={el.props.style} error={error !== undefined}>
          <label>{el.props.label}</label>
          <Input value={this.state.data[el.props.name].value}
            style={el.props.style}
            onChange={(_, d) => this.setState({
              data: {
                ...this.state.data, [el.props.name]: {
                  ...this.state.data[el.props.name],
                  value: d.value,
                },
              },
            })
            } />
          {error ? <Message error={true}>{error}</Message> : null}
        </Form.Field>;
      },
      submit => <Button disabled={submit.props.disabled || !this.validate()}
        onClick={() => this.submit(submit.props.onClick)}>
        {submit.props.children}
      </Button>)}
    </Form>;
  }

  submit(onClick: (data: { [index: string]: any }) => void) {
    const data: any = {};
    for (const key in this.state.data) {
      if (this.state.data.hasOwnProperty(key)) {
        const { type, value } = this.state.data[key];
        data[key] = type === "numeric" ? Number(value) : value;
      }
    }
    onClick(data);
  }

  getErrors(): { [index: string]: string } {
    const errors: { [index: string]: string } = {};
    for (const key in this.state.data) {
      if (this.state.data.hasOwnProperty(key)) {
        const { type, value, extra } = this.state.data[key];
        switch (type) {
          case "text":
            if (extra.nonEmpty && value === "") {
              errors[key] = "Must not be empty";
            }
            break;
          case "numeric":
            const num = Number(value);
            const min = extra.min;
            if (isNaN(num)) {
              errors[key] = "Not a number";
            }
            if (min !== undefined && num < min) {
              errors[key] = `Must not be less than ${min}`;
            }
            break;
        }
      }
    }
    return errors;
  }

  validate(): boolean {
    return LD.isEmpty(this.getErrors());
  }
}
