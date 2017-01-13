"use strict";

var rollup = require('rollup');

exports.rollupImpl = rollup.rollup;

exports.generateImpl = function(bundle, opts) {
  return bundle.generate(opts);
};

exports.writeImpl = function(bundle, opts) {
  return bundle.write(opts);
};
