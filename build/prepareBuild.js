const { existsSync, mkdirSync } = require('fs');

const { DIST_PATH, DIST_ASSETS_PATH } = require('./constants');

module.exports = async () => {
  if (!existsSync(DIST_PATH)) { mkdirSync(DIST_PATH); }
  if (!existsSync(DIST_ASSETS_PATH)) { mkdirSync(DIST_ASSETS_PATH); }
};
