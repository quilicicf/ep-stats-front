#!/usr/bin/env node

const statDir = require('./statDir');
const minifyApp = require('./minifyApp');
const copyAssets = require('./copyAssets');
const renderElm = require('./renderElm');
const renderSass = require('./renderSass');
const prepareBuild = require('./prepareBuild');

const { DIST_PATH, APP_OUTPUT_PATH } = require('./constants');

const main = async () => {
  await prepareBuild();
  copyAssets({ shouldWatch: false });
  await renderSass();
  await renderElm();
  await minifyApp(APP_OUTPUT_PATH);
  statDir(DIST_PATH);
};

main();
