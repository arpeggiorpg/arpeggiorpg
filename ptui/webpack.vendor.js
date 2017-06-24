const webpack = require('webpack');
const path = require('path');

module.exports = {
    entry: {
        vendor: ["react", "react-dom", 'react-panelgroup', 'lodash', 'svg-pan-zoom', 'hammerjs',
            'type-safe-json-decoder', 'immutable', 'redux', 'react-redux', 'isomorphic-fetch',
            'flexbox-react', 'Parsimmon', 'react-window-size-listener', 'semantic-ui-react']
    },
    output: {
        filename: "[name].js",
        path: __dirname + "/build",
        library: "[name]_dll",

    },
    stats: "errors-only",

    resolve: {
        // Add '.ts' and '.tsx' as resolvable extensions.
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    plugins: [
        new webpack.DllPlugin({
            path: __dirname + "/build/[name]-manifest.json",
            name: "[name]_dll",
        }),
        new webpack.optimize.ModuleConcatenationPlugin()
    ],
    module: {
        rules: [
            // All files with a '.ts' or '.tsx' extension will be handled by 'awesome-typescript-loader'.
            { test: /\.tsx?$/, loader: "ts-loader" },

            // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
            { enforce: "pre", test: /\.js$/, loader: "source-map-loader" }
        ]
    },
};
