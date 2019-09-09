#!/usr/bin/env node

const statDir = require('./statDir');
const copyAssets = require('./copyAssets');
const renderElm = require('./renderElm');
const renderSass = require('./renderSass');
const prepareBuild = require('./prepareBuild');
const gzipAppAndStyle = require('./gzipAppAndStyle');
const minifyAppAndStyle = require('./minifyAppAndStyle');

const main = async () => {
  await prepareBuild();
  await copyAssets({ isForProd: true });
  await renderSass({ isForProd: true });
  await renderElm({ isForProd: true });
  await minifyAppAndStyle();
  await gzipAppAndStyle();
  statDir();
};

main();
