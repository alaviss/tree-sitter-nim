# SPDX-FileCopyrightText: 2025 tree-sitter contributors
#
# SPDX-License-Identifier: MIT

from unittest import TestCase

import tree_sitter
import tree_sitter_nim


class TestLanguage(TestCase):
    def test_can_load_grammar(self):
        try:
            tree_sitter.Language(tree_sitter_nim.language())
        except Exception:
            self.fail("Error loading Nim grammar")
