#!/usr/bin/env node

const { watch } = require('chokidar');
const { resolve: resolvePath } = require('path');
const liveServer = require('live-server');

const renderElm = require('./renderElm');
const renderSass = require('./renderSass');
const prepareBuild = require('./prepareBuild');

const {
  SRC_PATH, DIST_PATH, APP_HTML_NAME, APP_HTML_SOURCE_PATH, APP_ENTRY_POINT, STYLE_ENTRY_POINT, STYLE_OUTPUT_PATH,
} = require('./constants');

const SERVER_PORT = 5420;
const ELM_FILES_GLOB = resolvePath(SRC_PATH, '**', '*.elm');
const STYLE_FILES_GLOB = resolvePath(SRC_PATH, '**', '*.scss');

const main = async () => {
  await prepareBuild();

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
    root: DIST_PATH,
    watch: [ SRC_PATH, APP_HTML_SOURCE_PATH, STYLE_OUTPUT_PATH ],
    wait: 500,
    file: APP_HTML_NAME,
  });
};

try {
  main();
} catch (error) {
  throw error;
}
