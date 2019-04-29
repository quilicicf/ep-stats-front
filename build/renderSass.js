const chalk = require('chalk');
const { render } = require('node-sass');

const writeFile = require('./writeFile');
const { STYLE_ENTRY_POINT, STYLE_OUTPUT_PATH, STYLE_SOURCE_MAP_PATH } = require('./constants');

const SASS_OPTIONS = {
  file: STYLE_ENTRY_POINT,
  outFile: STYLE_OUTPUT_PATH,
  sourceMap: true,
};

module.exports = async () => {
  try {
    const { css, map } = await new Promise((resolve, reject) => {
      render(SASS_OPTIONS, (error, result) => {
        if (error) {
          return reject(error);
        }
        return resolve(result);
      });
    });

    process.stdout.write('Success! Compiled SASS\n');
    writeFile(STYLE_OUTPUT_PATH, css);
    return writeFile(STYLE_SOURCE_MAP_PATH, map);

  } catch (sassRenderingError) {
    process.stdout.write(chalk.yellow(`SASS error:\n${sassRenderingError.stack}\n`));
    return Promise.resolve();
  }
};
