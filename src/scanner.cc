/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#include <bitset>
#include <cmath>
#include <cstdlib>
#include <exception>
#include <iostream>
#include <stdexcept>
#include <vector>

#include "tree_sitter/parser.h"

/// Token types understood by the parser. Keep up-to-date with grammar.js
enum TokenType { COMMENT, LONG_STRING_QUOTE, TERMINATOR, END_TOKEN_TYPE };
static constexpr auto START_TOKEN_TYPE = COMMENT;

/// Context for a scanning session.
///
/// This class manages the lexing context and abstracts over TSLexer.
class Context {
public:
  /// Creates a new Context.
  ///
  /// @param lexer - The lexer instance provided by tree-sitter. Must not be
  /// NULL.
  /// @param validsyms - The array of valid symbols provide by tree-sitter. Must
  /// not be NULL.
  Context(TSLexer *lexer, const bool *validsyms) : lexer(lexer) {
    for (int i = START_TOKEN_TYPE; i < END_TOKEN_TYPE; i++) {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      valid_symbols.set(i, validsyms[i]);
    }

    mark_end();
  };

  /// Returns whether the given token type is a valid token at this parsing
  /// state.
  [[nodiscard]] auto valid(enum TokenType type) const -> bool {
    return valid_symbols[type];
  }

  /// Returns the current lookahead symbol.
  [[nodiscard]] auto lookahead() const -> int32_t { return lexer->lookahead; }

  /// Advance the lexer.
  ///
  /// The advanced character will not be counted into the resulting token.
  /// Advancing will change the current state.
  ///
  /// @param skip - Whether to skip the token from the scanned range. Useful for
  /// spaces.
  /// @returns The next lookahead character.
  auto advance(bool skip = false) -> int32_t {
    state_ += static_cast<int>(eof());
    lexer->advance(lexer, skip);
    return lookahead();
  }

  /// Returns the currently tracked state.
  ///
  /// This state changes every time the lexer is advanced.
  auto state() const -> uint32_t { return state_; }

  /// Returns whether we are at EOF.
  auto eof() const -> bool { return lexer->eof(lexer); }

  /// Mark the current scanning position as the end of the resulting token.
  void mark_end() { lexer->mark_end(lexer); }

  /// Advances and mark the new position as the end.
  ///
  /// This effectively consumes the lookahead into the token.
  ///
  /// @param skip - See {@link advance}.
  /// @returns The next lookahead character.
  auto consume(bool skip = false) -> int32_t {
    const auto result = advance(skip);
    mark_end();
    return result;
  }

  /// Set the result symbol type and returns true.
  [[nodiscard]] auto finish(enum TokenType type) -> bool {
    lexer->result_symbol = type;
    return true;
  }

private:
  TSLexer *lexer;
  uint32_t state_{};
  std::bitset<3> valid_symbols;
};

// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TRY_LEX(ctx, fn)                                                       \
  {                                                                            \
    const auto tempState = (ctx).state();                                      \
    if (fn((ctx))) {                                                           \
      return true;                                                             \
    }                                                                          \
    if ((ctx).state() != tempState)                                            \
      return false;                                                            \
  }

/// Handle parsing LONG_STRING_QUOTE.
///
/// Will not advance the lexer if the token is not valid at the current
/// position.
static auto lex_long_string_quote(Context &ctx) -> bool {
  if (ctx.valid(LONG_STRING_QUOTE) && ctx.lookahead() == '"') {
    ctx.consume();
    uint8_t quotes = 1;
    while (ctx.lookahead() != '"' && quotes < 3) {
      ctx.advance();
      quotes++;
    }

    if (quotes < 3) {
      ctx.mark_end();
      return ctx.finish(LONG_STRING_QUOTE);
    }

    return ctx.lookahead() == '"';
  }

  return false;
}

/// Handle lexing tasks that has to be done before any whitespace skipping
/// happens.
static auto lex_immediate(Context &ctx) -> bool {
  return lex_long_string_quote(ctx);
}

/// Handle simple lexing rules. These do not perform more than 1 character
/// lookahead.
///
/// Will not advance the lexer if the tokens did not match.
static auto lex_simple(Context &ctx) -> bool {
  if (ctx.valid(TERMINATOR) &&
      (ctx.eof() || ctx.lookahead() == '\n' || ctx.lookahead() == '\r')) {
    ctx.consume();
    return ctx.finish(TERMINATOR);
  }

  return false;
}

/// Fetch the current line as a comment.
///
/// When finished, will set the symbol to COMMENT and returns true.
static auto get_comment_line(Context &ctx) -> bool {
  while (!ctx.eof() && (ctx.lookahead() != '\n' || ctx.lookahead() != '\r')) {
    ctx.consume();
  }
  return ctx.finish(COMMENT);
}

enum CommentMarkerType {
  /// Invalid comment marker, used for errors.
  CMT_INVALID,
  /// Tokens for short comments.
  CMT_SHORT,
  /// Tokens for long comments.
  CMT_LONG,
  /// Tokens for long documentation comments.
  CMT_DOC_LONG
};

/// Assuming the next character is `#`.
///
/// Classify what type of comment does the marker is used to start and consumes
/// it.
static auto get_comment_start(Context &ctx) -> CommentMarkerType {
  if (ctx.lookahead() != '#') {
    return CMT_INVALID;
  }

  switch (ctx.consume()) {
  case '#':
    if (ctx.consume() == '[') {
      ctx.consume();
      return CMT_DOC_LONG;
    } else {
      return CMT_SHORT;
    }
  case '[':
    ctx.consume();
    return CMT_LONG;
  }

  return CMT_SHORT;
}

/// Assuming the next character is `]`. Returns whether the next few characters
/// marks the end of the given comment type. Characters are consumed whether or
/// not they actually ends the comment.
static auto get_comment_end(Context &ctx, CommentMarkerType type) -> bool {
  if (type == CMT_SHORT || type == CMT_INVALID) {
    return false;
  }
  if (ctx.lookahead() != ']') {
    return false;
  }

  if (ctx.consume() == '#') {
    if (type == CMT_LONG) {
      ctx.consume();
      return true;
    }

    if (ctx.consume() == '#') {
      ctx.consume();
      return true;
    }
  }

  return false;
}

/// Handle Nim's long and short comments.
static auto lex_comment(Context &ctx) -> bool {
  if (ctx.valid(COMMENT) && ctx.lookahead() == '#') {
    std::vector<bool> comment_stack;
    switch (get_comment_start(ctx)) {
    case CMT_SHORT:
      return get_comment_line(ctx);
    case CMT_LONG:
      comment_stack.push_back(false);
      break;
    case CMT_DOC_LONG:
      comment_stack.push_back(true);
      break;
    default:
      return false;
    }

    while (!comment_stack.empty()) {
      switch (ctx.lookahead()) {
      case '#': {
        switch (get_comment_start(ctx)) {
        case CMT_LONG:
          comment_stack.push_back(false);
          break;
        case CMT_DOC_LONG:
          comment_stack.push_back(true);
          break;
        case CMT_SHORT:
          break;
        default:
          return false;
        }
        break;
      }
      case ']': {
        CommentMarkerType endType =
            comment_stack.back() ? CMT_DOC_LONG : CMT_LONG;
        if (get_comment_end(ctx, endType)) {
          comment_stack.pop_back();
        }
        break;
      }
      case '\0':
        return false;
      default:
        ctx.consume();
      }
    }

    return ctx.finish(COMMENT);
  }

  return false;
}

/// Skip whitespaces.
static void skip(Context &ctx) {
  while (ctx.lookahead() == ' ') {
    ctx.advance(true);
  }
}

extern "C" {
auto tree_sitter_nim_external_scanner_create() noexcept -> void * {
  return nullptr;
}
void tree_sitter_nim_external_scanner_destroy(void *payload) noexcept {}
auto tree_sitter_nim_external_scanner_serialize(void * /*payload*/,
                                                char * /*buffer*/) noexcept
    -> unsigned {
  return 0;
}
void tree_sitter_nim_external_scanner_deserialize(void *payload,
                                                  const char *buffer,
                                                  unsigned length) noexcept {}
auto tree_sitter_nim_external_scanner_scan(void * /*payload*/, TSLexer *lexer,
                                           const bool *valid_symbols) noexcept
    -> bool {
  auto ctx = Context(lexer, valid_symbols);

  TRY_LEX(ctx, lex_immediate);

  skip(ctx);

  TRY_LEX(ctx, lex_simple);
  TRY_LEX(ctx, lex_comment);

  return false;
}
}
