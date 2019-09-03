const _ = require('lodash');
const getTree = require('directory-tree');
const fileSize = require('filesize');

const { DIST_PATH } = require('./constants');

const INDENTATIONS = {
  CHILD: '├── ',
  LAST_CHILD: '└── ',
  GRAND_CHILD: '│   ',
};

const displayNode = (node) => {
  process.stdout.write(`${node.name} (${fileSize(node.size, { round: 1 })})\n`);
};

const recursiveShowTree = (tree, depth = 0) => {
  if (depth === 0) { displayNode(tree); }
  if (_.isEmpty(tree)) { return; }

  const treeChildrenNumber = _.size(tree);

  _.each(tree.children, (child, index) => {
    const grandChildIndentation = _.padStart('', depth * _.size(INDENTATIONS.GRAND_CHILD), '│   ');
    process.stdout.write(grandChildIndentation);
    process.stdout.write(index < treeChildrenNumber ? INDENTATIONS.CHILD : INDENTATIONS.LAST_CHILD);
    displayNode(child);

    recursiveShowTree(child, depth + 1);
  });
};
module.exports = () => {
  const folderTree = getTree(DIST_PATH, { exclude: /netlify\.toml/ });
  recursiveShowTree(folderTree, 0);
};
