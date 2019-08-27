#!/usr/bin/env node

const copyAssets = require('./copyAssets');
const renderElm = require('./renderElm');
const renderSass = require('./renderSass');
const prepareBuild = require('./prepareBuild');


const main = async () => {
  await prepareBuild();
  copyAssets({ shouldWatch: false });
  await renderSass();
  await renderElm();
};

main();
