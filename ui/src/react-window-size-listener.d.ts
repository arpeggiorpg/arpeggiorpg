declare module 'react-window-size-listener' {
    interface WindowSizeProps {
        onResize: (x: {windowWidth: number, windowHeight: number}) => void;
    }
    const WindowSizeListener: React.ComponentClass<WindowSizeProps>;
    export default WindowSizeListener;
}
