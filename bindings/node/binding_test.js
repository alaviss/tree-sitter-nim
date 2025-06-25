// SPDX-FileCopyrightText: 2024 tree-sitter contributors
//
// SPDX-License-Identifier: MIT

const assert = require("node:assert");
const { test } = require("node:test");

const Parser = require("tree-sitter");

test("can load grammar", () => {
  const parser = new Parser();
  assert.doesNotThrow(() => parser.setLanguage(require(".")));
});
