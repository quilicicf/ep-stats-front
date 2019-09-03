const through = require('through');

function changeImport () {
  return through(
    function write (chunk) {
      this.queue(chunk.toString().replace(/script src="app\.js"/, 'script src="app.min.js"'));
    },
    function end () {
      this.queue(null);
    },
  );
}

module.exports = changeImport;
