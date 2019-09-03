const through = require('through');

function changeImports () {
  return through(
    function write (chunk) {
      this.queue(chunk.toString()
        .replace(/script src="app\.js"/, 'script src="app.min.js"')
        .replace(/rel="stylesheet" href="app\.css"/, 'rel="stylesheet" href="app.min.css"'));
    },
    function end () {
      this.queue(null);
    },
  );
}

module.exports = changeImports;
