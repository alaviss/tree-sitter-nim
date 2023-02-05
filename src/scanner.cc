/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#include "tree_sitter/parser.h"

/// Token types understood by the parser. Keep up-to-date with grammar.js
enum TokenType { LONG_STRING_QUOTE, TERMINATOR };

extern "C" {
void *tree_sitter_nim_external_scanner_create() { return nullptr; }
void tree_sitter_nim_external_scanner_destroy(void *payload) {}
unsigned tree_sitter_nim_external_scanner_serialize(void *payload,
                                                    char *buffer) {
  return 0;
}
void tree_sitter_nim_external_scanner_deserialize(void *payload,
                                                  const char *buffer,
                                                  unsigned length) {}
bool tree_sitter_nim_external_scanner_scan(void *payload, TSLexer *lexer,
                                           const bool *valid_symbols) {
  if (valid_symbols[LONG_STRING_QUOTE] && lexer->lookahead == '"') {
    lexer->result_symbol = LONG_STRING_QUOTE;

    lexer->advance(lexer, false);
    lexer->mark_end(lexer);

    uint8_t count = 1;
    while (lexer->lookahead == '"' && count < 3) {
      lexer->advance(lexer, false);
      count++;
    }

    if (count < 3) {
      lexer->mark_end(lexer);
      return true;
    } else if (lexer->lookahead != '"')
      return false;
    else
      return true;
  }

  if (valid_symbols[TERMINATOR] &&
      (lexer->eof(lexer) || lexer->lookahead == '\n' ||
       lexer->lookahead == '\r')) {
    lexer->result_symbol = TERMINATOR;
    lexer->advance(lexer, false);
    return true;
  }

  return false;
}
}
