const liveServer = require('live-server');

const {
  DIST_PATH, APP_HTML_OUTPUT_PATH, STYLE_OUTPUT_PATH, APP_HTML_NAME,
} = require('./constants');

const SERVER_PORT = 5420;

module.exports = () => {
  liveServer.start({
    host: 'localhost',
    port: SERVER_PORT,
    root: DIST_PATH,
    open: false,
    watch: [ APP_HTML_OUTPUT_PATH, STYLE_OUTPUT_PATH ],
    wait: 500,
    file: APP_HTML_NAME,
  });
};
