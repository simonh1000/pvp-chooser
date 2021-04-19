const path = require('path');

const { merge } = require('webpack-merge');
const common = require('./webpack.common.js');


const dev = {
    mode: 'development',
    devServer: {
        inline: true,
        hot: true,
        stats: "errors-only",
        contentBase: path.join(__dirname, "../src/assets"),
        publicPath: "/",
        historyApiFallback: true
    },
};

module.exports = env => {
    const withDebug = !env.nodebug;
    console.log("withDebug", withDebug);
    return merge(common(withDebug), dev);
}
