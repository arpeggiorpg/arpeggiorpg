const webpack = require('webpack');
const path = require('path');

module.exports = {
    entry: {
        vendor: [
            'flexbox-react',
            'hammerjs',
            'immutable',
            'isomorphic-fetch',
            'lodash',
            'Parsimmon',
            'react-color',
            'react-dom',
            'react-panelgroup',
            'react-redux',
            'react-window-size-listener',
            'react',
            'redux',
            'redux-thunk',
            'reselect',
            'semantic-ui-react',
            'svg-pan-zoom',
            'type-safe-json-decoder'
        ]
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
