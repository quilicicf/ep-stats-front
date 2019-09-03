const _ = require('lodash');
const { copy, watch } = require('cpx');
const { cyan, yellow } = require('chalk');
const { relative } = require('path');

const changeAppImport = require('./changeAppImport');

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
  { source: APP_HTML_SOURCE_PATH, destination: DIST_PATH, transformer: changeAppImport },
  { source: DEPLOYMENT_SOURCE_PATH, destination: DIST_PATH },
  { source: ASSETS_GLOB, destination: DIST_ASSETS_PATH },
  { source: WEB_FONT_SOURCE_GLOB, destination: DIST_ASSETS_PATH },
];

const relativize = path => relative(DIR_PATH, path);

const watchAssets = () => {
  _.each(WATCH_LIST, ({ source, destination }) => {
    process.stdout.write(`Watcher initialized: ${cyan(relativize(source))} => ${yellow(relativize(destination))}\n`);
    watch(source, destination, { initialCopy: true })
      .on('copy', ({ srcPath }) => {
        process.stdout.write(`${cyan('Just copied:')} ${relativize(srcPath)}\n`);
      });
  });
};

const copyAssets = () => {
  const copyPromises = _.map(
    WATCH_LIST,
    ({ source, destination, transformer }) => new Promise((resolve, reject) => {
      process.stdout.write(`File(s) copied: ${cyan(relativize(source))} => ${yellow(relativize(destination))}\n`);
      copy(source, destination, { initialCopy: true, transform: transformer }, (error) => {
        if (error) { return reject(error); }
        return resolve();
      });
    }),
  );

  return Promise.all(copyPromises);
};

module.exports = async ({ shouldWatch = false }) => (shouldWatch ? watchAssets() : copyAssets());
