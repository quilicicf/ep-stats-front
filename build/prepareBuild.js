const { existsSync, mkdirSync, copyFileSync } = require('fs');

const writeFile = require('./writeFile');
const {
  DIST_PATH,
  STYLE_OUTPUT_PATH,
  STYLE_SOURCE_MAP_PATH,
  APP_HTML_SOURCE_PATH,
  APP_HTML_OUTPUT_PATH,
  DEPLOYMENT_SOURCE_PATH,
  DEPLOYMENT_OUTPUT_PATH,
} = require('./constants');

module.exports = async () => {
  if (!existsSync(DIST_PATH)) { mkdirSync(DIST_PATH); }
  copyFileSync(APP_HTML_SOURCE_PATH, APP_HTML_OUTPUT_PATH);
  copyFileSync(DEPLOYMENT_SOURCE_PATH, DEPLOYMENT_OUTPUT_PATH);

  if (!existsSync(STYLE_OUTPUT_PATH)) { await writeFile(STYLE_OUTPUT_PATH, ''); }
  if (!existsSync(STYLE_SOURCE_MAP_PATH)) { await writeFile(STYLE_SOURCE_MAP_PATH, ''); }
};
