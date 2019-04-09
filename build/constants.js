const { resolve: resolvePath } = require('path');

const DIR_PATH = resolvePath(__dirname, '..');
const SRC_PATH = resolvePath(DIR_PATH, 'src');
const DIST_PATH = resolvePath(DIR_PATH, 'dist');

module.exports = {
  DIR_PATH,
  SRC_PATH,
  APP_ENTRY_POINT: resolvePath(SRC_PATH, 'Main.elm'),
  STYLE_ENTRY_POINT: resolvePath(SRC_PATH, 'app.scss'),

  APP_OUTPUT_PATH: resolvePath(DIST_PATH, 'app.js'),
  STYLE_OUTPUT_PATH: resolvePath(DIST_PATH, 'app.css'),
};
