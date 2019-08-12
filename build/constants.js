const { resolve: resolvePath } = require('path');

const DIR_PATH = resolvePath(__dirname, '..');
const SRC_PATH = resolvePath(DIR_PATH, 'src');
const DIST_PATH = resolvePath(DIR_PATH, 'dist');
const ASSETS_PATH = resolvePath(DIST_PATH, 'assets');
const NODE_MODULES_PATH = resolvePath(__dirname, '..', 'node_modules');

const APP_HTML_NAME = 'index.html';
const DEPLOYMENT_NAME = 'netlify.toml';

module.exports = {
  DIR_PATH,
  DIST_PATH,
  ASSETS_PATH,
  APP_HTML_NAME,

  SRC_PATH,
  APP_HTML_SOURCE_PATH: resolvePath(DIR_PATH, APP_HTML_NAME),
  APP_ENTRY_POINT: resolvePath(SRC_PATH, 'Main.elm'),
  STYLE_ENTRY_POINT: resolvePath(SRC_PATH, 'app.scss'),
  DEPLOYMENT_SOURCE_PATH: resolvePath(DIR_PATH, DEPLOYMENT_NAME),
  WEB_FONT_SOURCE_GLOB: resolvePath(NODE_MODULES_PATH, '@fortawesome', 'fontawesome-free-webfonts', 'webfonts', 'fa-solid-900*'),

  APP_HTML_OUTPUT_PATH: resolvePath(DIST_PATH, APP_HTML_NAME),
  APP_OUTPUT_PATH: resolvePath(DIST_PATH, 'app.js'),
  STYLE_OUTPUT_PATH: resolvePath(DIST_PATH, 'app.css'),
  STYLE_SOURCE_MAP_PATH: resolvePath(DIST_PATH, 'app.css.map'),
  DEPLOYMENT_OUTPUT_PATH: resolvePath(DIST_PATH, DEPLOYMENT_NAME),
};
