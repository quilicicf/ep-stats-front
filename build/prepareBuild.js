const { existsSync, mkdirSync } = require('fs');

const writeFile = require('./writeFile');
const {
  DIST_PATH, DIST_ASSETS_PATH, STYLE_OUTPUT_PATH, STYLE_SOURCE_MAP_PATH,
} = require('./constants');

module.exports = async () => {
  if (!existsSync(DIST_PATH)) { mkdirSync(DIST_PATH); }
  if (!existsSync(DIST_ASSETS_PATH)) { mkdirSync(DIST_ASSETS_PATH); }
  if (!existsSync(STYLE_OUTPUT_PATH)) { await writeFile(STYLE_OUTPUT_PATH, ''); }
  if (!existsSync(STYLE_SOURCE_MAP_PATH)) { await writeFile(STYLE_SOURCE_MAP_PATH, ''); }
};
