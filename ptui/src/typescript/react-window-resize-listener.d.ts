declare module 'react-window-resize-listener' {
    interface WindowResizeProps {
        onResize: (x: {windowWidth: number, windowHeight: number}) => void;
    }
    export const WindowResizeListener: React.ComponentClass<WindowResizeProps>;
}
