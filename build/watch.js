#!/usr/bin/env node

const { watch } = require('chokidar');
const { resolve: resolvePath } = require('path');

const copyAssets = require('./copyAssets');
const renderElm = require('./renderElm');
const renderSass = require('./renderSass');
const startServer = require('./startServer');
const prepareBuild = require('./prepareBuild');

const { SRC_PATH, APP_ENTRY_POINT, STYLE_ENTRY_POINT } = require('./constants');

const ELM_FILES_GLOB = resolvePath(SRC_PATH, '**', '*.elm');
const STYLE_FILES_GLOB = resolvePath(SRC_PATH, '**', '*.scss');

const elmTaskQueue = Promise.resolve();
const addElmRenderToTaskQueue = () => elmTaskQueue.then(() => renderSass({}));

const main = async () => {
  await prepareBuild();
  await copyAssets({});

  const elmWatcher = watch(
    [ APP_ENTRY_POINT, ELM_FILES_GLOB ],
    { cwd: SRC_PATH, ignoreInitial: true },
  );

  elmWatcher
    .on('add', renderElm)
    .on('change', renderElm)
    .on('unlink', renderElm)
    .on('ready', renderElm);

  const sassWatcher = watch(
    [ STYLE_ENTRY_POINT, STYLE_FILES_GLOB ],
    { cwd: SRC_PATH, ignoreInitial: true },
  );

  sassWatcher
    .on('add', addElmRenderToTaskQueue)
    .on('change', addElmRenderToTaskQueue)
    .on('unlink', addElmRenderToTaskQueue)
    .on('ready', addElmRenderToTaskQueue);

  startServer();
};

main();
