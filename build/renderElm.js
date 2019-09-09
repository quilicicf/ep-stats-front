const { compile } = require('node-elm-compiler');

const { DIR_PATH, APP_ENTRY_POINT, APP_OUTPUT_PATH } = require('./constants');

module.exports = async ({ isForProd = false }) => new Promise((resolve, reject) => {
  compile([ APP_ENTRY_POINT ], {
    output: APP_OUTPUT_PATH,
    debug: !isForProd,
    cwd: DIR_PATH,
  }).on('close', (exitCode) => {
    if (exitCode !== 0) { return reject(Error('Elm compilation failed!')); }
    return resolve();
  });
});
