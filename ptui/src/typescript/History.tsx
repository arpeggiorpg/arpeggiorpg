import * as React from "react";
import * as ReactDOM from "react-dom";

interface HistoryProps extends React.Props<any> {
  data: any;

}

class History extends React.Component<HistoryProps, any> {

  render() {
    return <div>Hello, world! {this.props.data}</div>;
  }
}


export function renderHistory([id, data]: [string, any]) {
  ReactDOM.render(
    <History data={data} />,
    document.getElementById(id)
  );
}
