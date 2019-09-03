const postcss = require('postcss');
const { cyan } = require('chalk');
const { minify } = require('uglify-js');
const cssnano = require('cssnano')({ preset: 'default' });

const { APP_OUTPUT_PATH, APP_MIN_OUTPUT_PATH, STYLE_OUTPUT_PATH, STYLE_MIN_OUTPUT_PATH } = require('./constants');
const writeFile = require('./writeFile');
const readFile = require('./readFile');
const readBuffer = require('./readBuffer');

/**
 * See https://guide.elm-lang.org/optimization/asset_size.html
 */
const minifyApp = async () => {
  const fileContent = await readFile(APP_OUTPUT_PATH);
  const { error, warnings, code: minifiedCode } = minify(fileContent, {
    compress: {
      pure_funcs: 'F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9',
      pure_getters: true,
      keep_fargs: false,
      unsafe: true,
      unsafe_comps: true,
    },
    mangle: true,
  });

  if (error) { throw error; }
  if (warnings) { process.stderr.write(`${warnings}\n`); }

  await writeFile(APP_MIN_OUTPUT_PATH, minifiedCode);
};

const minifyStyle = async () => {
  const style = await readBuffer(STYLE_OUTPUT_PATH);
  const { css: minifiedStyle } = await postcss([ cssnano ])
    .process(style, { from: STYLE_OUTPUT_PATH, to: STYLE_MIN_OUTPUT_PATH });

  await writeFile(STYLE_MIN_OUTPUT_PATH, minifiedStyle);
};

module.exports = async () => {
  process.stdout.write(`${cyan('Minifying app...')}\n`);
  await minifyApp();

  process.stdout.write(`${cyan('Minifying style...')}\n`);
  await minifyStyle();
};
