#!/usr/bin/env node

const { watch } = require('chokidar');
const { resolve: resolvePath } = require('path');
const liveServer = require('live-server');

const renderElm = require('./renderElm');
const renderSass = require('./renderSass');

const {
  SRC_PATH, DIR_PATH, APP_HTML_PATH, APP_ENTRY_POINT, STYLE_ENTRY_POINT,
} = require('./constants');

const SERVER_PORT = 5420;
const ELM_FILES_GLOB = resolvePath(SRC_PATH, '**', '*.elm');
const STYLE_FILES_GLOB = resolvePath(SRC_PATH, '**', '*.scss');

const main = async () => {
  const elmWatcher = watch(
    [ APP_ENTRY_POINT, ELM_FILES_GLOB ],
    { cwd: SRC_PATH, ignoreInitial: true },
  );

  elmWatcher
    .on('add', renderElm)
    .on('change', renderElm)
    // .on('unlink', renderElm) TODO needed?
    .on('ready', renderElm);

  const sassWatcher = watch(
    [ STYLE_ENTRY_POINT, STYLE_FILES_GLOB ],
    { cwd: SRC_PATH, ignoreInitial: true },
  );

  sassWatcher
    .on('add', renderSass)
    .on('change', renderSass)
    // .on('unlink', renderSass) TODO needed?
    .on('ready', renderSass);

  liveServer.start({
    port: SERVER_PORT,
    root: DIR_PATH,
    watch: `${SRC_PATH},${APP_HTML_PATH}`,
    open: false,
    wait: 500,
  });
};

try {
  main();
} catch (error) {
  throw error;
}
