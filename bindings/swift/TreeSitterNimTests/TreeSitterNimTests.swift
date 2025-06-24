// SPDX-FileCopyrightText: 2025 tree-sitter contributors
//
// SPDX-License-Identifier: MIT

import XCTest
import SwiftTreeSitter
import TreeSitterNim

final class TreeSitterNimTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_nim())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Nim grammar")
    }
}
