const _ = require('lodash');
const { readFile } = require('fs');

module.exports = async filePath => new Promise((resolve, reject) => {
  readFile(filePath, (error, data) => {
    if (error) { return reject(error); }
    return resolve(data);
  });
});
