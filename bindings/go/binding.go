// SPDX-FileCopyrightText: 2024 tree-sitter contributors
//
// SPDX-License-Identifier: MIT

package tree_sitter_nim

// #cgo CFLAGS: -std=c11 -fPIC
// #include "../../src/parser.c"
// #include "../../src/scanner.c"
import "C"

import "unsafe"

// Get the tree-sitter Language for this grammar.
func Language() unsafe.Pointer {
	return unsafe.Pointer(C.tree_sitter_nim())
}
