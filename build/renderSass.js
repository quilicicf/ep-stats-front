const chalk = require('chalk');
const { render } = require('node-sass');

const writeFile = require('./writeFile');
const { STYLE_ENTRY_POINT, STYLE_OUTPUT_PATH, STYLE_SOURCE_MAP_PATH } = require('./constants');

const getSassOptions = isForProd => ({
  file: STYLE_ENTRY_POINT,
  outFile: STYLE_OUTPUT_PATH,
  precision: 9,
  sourceMap: !isForProd,
  outputStyle: isForProd ? 'compress' : 'nested',
});

module.exports = async ({ isForProd = false }) => {
  try {
    const sassOptions = getSassOptions(isForProd);
    const { css, map } = await new Promise((resolve, reject) => {
      render(sassOptions, (error, result) => {
        if (error) {
          return reject(error);
        }
        return resolve(result);
      });
    });

    process.stdout.write('Success! Compiled SASS\n');
    await writeFile(STYLE_OUTPUT_PATH, css);
    if (map) { await writeFile(STYLE_SOURCE_MAP_PATH, map); }

    return Promise.resolve();

  } catch (sassRenderingError) {
    process.stdout.write(chalk.yellow(`SASS error:\n${sassRenderingError.stack}\n`));
    return Promise.resolve();
  }
};
