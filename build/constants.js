const { resolve: resolvePath } = require('path');

const DIR_PATH = resolvePath(__dirname, '..');
const SRC_PATH = resolvePath(DIR_PATH, 'src');
const DIST_PATH = resolvePath(DIR_PATH, 'dist');
const APP_HTML_NAME = 'index.html';

module.exports = {
  DIR_PATH,
  APP_HTML_NAME,
  APP_HTML_PATH: resolvePath(DIR_PATH, APP_HTML_NAME),

  SRC_PATH,
  APP_ENTRY_POINT: resolvePath(SRC_PATH, 'Main.elm'),
  STYLE_ENTRY_POINT: resolvePath(SRC_PATH, 'app.scss'),

  APP_OUTPUT_PATH: resolvePath(DIST_PATH, 'app.js'),
  STYLE_OUTPUT_PATH: resolvePath(DIST_PATH, 'app.css'),
  STYLE_SOURCE_MAP_PATH: resolvePath(DIST_PATH, 'app.css.map'),
};
