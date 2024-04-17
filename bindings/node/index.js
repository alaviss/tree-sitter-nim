// SPDX-FileCopyrightText: 2024 tree-sitter contributors
//
// SPDX-License-Identifier: MIT

const root = require("path").join(__dirname, "..", "..");

module.exports = require("node-gyp-build")(root);

try {
  module.exports.nodeTypeInfo = require("../../src/node-types.json");
} catch (_) {}
