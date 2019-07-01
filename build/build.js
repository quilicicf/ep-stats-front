#!/usr/bin/env node

const renderElm = require('./renderElm');
const renderSass = require('./renderSass');
const prepareBuild = require('./prepareBuild');


const main = async () => {
  await prepareBuild();
  await renderSass();
  await renderElm();
};

try {
  main();
} catch (error) {
  throw error;
}
