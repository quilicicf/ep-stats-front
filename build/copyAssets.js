const _ = require('lodash');
const { copySync, watch } = require('cpx');
const { cyan, yellow } = require('chalk');
const { relative } = require('path');

const {
  DIR_PATH,
  DIST_PATH,
  DIST_ASSETS_PATH,
  APP_HTML_SOURCE_PATH,
  DEPLOYMENT_SOURCE_PATH,
  ASSETS_GLOB,
  WEB_FONT_SOURCE_GLOB,
} = require('./constants');

const WATCH_LIST = [
  { source: APP_HTML_SOURCE_PATH, destination: DIST_PATH },
  { source: DEPLOYMENT_SOURCE_PATH, destination: DIST_PATH },
  { source: ASSETS_GLOB, destination: DIST_ASSETS_PATH },
  { source: WEB_FONT_SOURCE_GLOB, destination: DIST_ASSETS_PATH },
];

const relativize = path => relative(DIR_PATH, path);

const createWatcher = (source, destination, options) =>
  watch(source, destination, options)
    .on('copy', ({ srcPath }) => {
      process.stdout.write(`${cyan('Just copied:')} ${relativize(srcPath)}\n`);
    });

module.exports = ({ shouldWatch = false }) => {
  const method = shouldWatch ? createWatcher : copySync;

  _.each(WATCH_LIST, ({ source, destination }) => {
    process.stdout.write(`Watcher initialized: ${cyan(relativize(source))} => ${yellow(relativize(destination))}\n`);
    method(source, destination, { initialCopy: true });
  });
};
