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

const renderTaskQueue = Promise.resolve();
const addElmRenderToTaskQueue = () => renderTaskQueue.then(() => renderElm({}));
const addSassRenderToTaskQueue = () => renderTaskQueue.then(() => renderSass({}));

const main = async () => {
  await prepareBuild();
  await copyAssets({});

  const elmWatcher = watch(
    [ APP_ENTRY_POINT, ELM_FILES_GLOB ],
    { cwd: SRC_PATH, ignoreInitial: true },
  );

  elmWatcher
    .on('add', addElmRenderToTaskQueue)
    .on('change', addElmRenderToTaskQueue)
    .on('unlink', addElmRenderToTaskQueue)
    .on('ready', addElmRenderToTaskQueue);

  const sassWatcher = watch(
    [ STYLE_ENTRY_POINT, STYLE_FILES_GLOB ],
    { cwd: SRC_PATH, ignoreInitial: true },
  );

  sassWatcher
    .on('add', addSassRenderToTaskQueue)
    .on('change', addSassRenderToTaskQueue)
    .on('unlink', addSassRenderToTaskQueue)
    .on('ready', addSassRenderToTaskQueue);

  startServer();
};

main();
