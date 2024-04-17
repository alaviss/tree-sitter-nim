// SPDX-FileCopyrightText: 2024 tree-sitter contributors
//
// SPDX-License-Identifier: MIT

package tree_sitter_nim_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-nim"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_nim.Language())
	if language == nil {
		t.Errorf("Error loading Nim grammar")
	}
}
