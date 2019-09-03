const { cyan } = require('chalk');
const { execSync } = require('child_process');

const {
  APP_MIN_OUTPUT_PATH, APP_GZIPPED_OUTPUT_PATH, STYLE_MIN_OUTPUT_PATH, STYLE_GZIPPED_OUTPUT_PATH,
} = require('./constants');

module.exports = () => {
  process.stdout.write(`${cyan('GZipping...')}\n`);
  execSync(`gzip -9 -c < ${APP_MIN_OUTPUT_PATH} > ${APP_GZIPPED_OUTPUT_PATH}`);
  execSync(`gzip -9 -c < ${STYLE_MIN_OUTPUT_PATH} > ${STYLE_GZIPPED_OUTPUT_PATH}`);
};
