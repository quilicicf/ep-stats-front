const _ = require('lodash');
const fastGlob = require('fast-glob');
const { basename, resolve: resolvePath } = require('path');
const { existsSync, mkdirSync, copyFileSync } = require('fs');

const writeFile = require('./writeFile');
const {
  DIST_PATH,
  ASSETS_PATH,
  STYLE_OUTPUT_PATH,
  STYLE_SOURCE_MAP_PATH,
  APP_HTML_SOURCE_PATH,
  APP_HTML_OUTPUT_PATH,
  DEPLOYMENT_SOURCE_PATH,
  DEPLOYMENT_OUTPUT_PATH,
  WEB_FONT_SOURCE_GLOB,
} = require('./constants');

module.exports = async () => {
  if (!existsSync(DIST_PATH)) { mkdirSync(DIST_PATH); }
  if (!existsSync(ASSETS_PATH)) { mkdirSync(ASSETS_PATH); }
  copyFileSync(APP_HTML_SOURCE_PATH, APP_HTML_OUTPUT_PATH);
  copyFileSync(DEPLOYMENT_SOURCE_PATH, DEPLOYMENT_OUTPUT_PATH);
  _.each(await fastGlob(WEB_FONT_SOURCE_GLOB), file => {
    const fileName = basename(file);
    copyFileSync(file, resolvePath(ASSETS_PATH, fileName));
  });

  if (!existsSync(STYLE_OUTPUT_PATH)) { await writeFile(STYLE_OUTPUT_PATH, ''); }
  if (!existsSync(STYLE_SOURCE_MAP_PATH)) { await writeFile(STYLE_SOURCE_MAP_PATH, ''); }
};
