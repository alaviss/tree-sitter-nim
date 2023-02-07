/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#include <algorithm>
#include <array>
#include <bitset>
#include <cstdint>
#include <limits>
#include <utility>

#include "tree_sitter/parser.h"

namespace {

using namespace std;

enum class TokenType : TSSymbol {
  TokenTypeStart,
  Comment = TokenTypeStart,
  LongStringQuote,
  Terminator,
  Colon,
  Equal,
  BinaryOpStart,
  BinaryOp10Left = BinaryOpStart,
  BinaryOp10Right,
  BinaryOp9,
  BinaryOp8,
  BinaryOp7,
  BinaryOp6,
  BinaryOp5,
  BinaryOp2,
  BinaryOp1,
  BinaryOp0,
  TokenTypeLen
};

using ValidSymbols = bitset<static_cast<size_t>(TokenType::TokenTypeLen)>;

constexpr ValidSymbols make_valid_symbols(initializer_list<TokenType> syms)
{
  uint64_t result{};
  for (const auto sym : syms) {
    result |= 1U << static_cast<uint8_t>(sym);
  }
  return result;
}

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
  Context(TSLexer* lexer, const bool* validsyms) : lexer_(lexer)
  {
    for (auto i = (int)TokenType::TokenTypeStart;
         i < (int)TokenType::TokenTypeLen; i++) {
      // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      valid_.set(i, validsyms[i]);
    }

    mark_end();
  };

  /// Returns whether the given token type is a valid token at this parsing
  /// state.
  [[nodiscard]] bool valid(enum TokenType type) const
  {
    return valid_.test(static_cast<size_t>(type));
  }

  [[nodiscard]] bool valid(ValidSymbols sym) const
  {
    return (valid_ & sym).any();
  }

  /// Returns whether tree-sitter is in error recovery mode.
  [[nodiscard]] bool error() const { return valid_.all(); }

  /// Returns the current lookahead symbol.
  [[nodiscard]] char32_t lookahead() const { return lexer_->lookahead; }

  /// Advance the lexer.
  ///
  /// The advanced character will not be counted into the resulting token.
  /// Advancing will change the current state.
  ///
  /// @param skip - Whether to skip the token from the scanned range. Useful for
  /// spaces.
  /// @returns The next lookahead character.
  char32_t advance(bool skip = false)
  {
    state_ += (int)eof();
    lexer_->advance(lexer_, skip);
    return lookahead();
  }

  /// Returns the currently tracked state.
  ///
  /// This state changes every time the lexer is advanced.
  uint32_t state() const { return state_; }

  /// Returns whether we are at EOF.
  bool eof() const { return lexer_->eof(lexer_); }

  /// Mark the current scanning position as the end of the resulting token.
  void mark_end() { lexer_->mark_end(lexer_); }

  /// Advances and mark the new position as the end.
  ///
  /// This effectively consumes the lookahead into the token.
  ///
  /// @param skip - See {@link advance}.
  /// @returns The next lookahead character.
  char32_t consume(bool skip = false)
  {
    const auto result = advance(skip);
    mark_end();
    return result;
  }

  /// Set the result symbol type and returns true.
  [[nodiscard]] bool finish(enum TokenType type)
  {
    lexer_->result_symbol = (TSSymbol)type;
    return true;
  }

private:
  TSLexer* lexer_;
  uint32_t state_{};
  ValidSymbols valid_;
};

// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TRY_LEX(ctx, fn)                          \
  {                                               \
    const auto tempState = (ctx).state();         \
    if (fn((ctx))) {                              \
      return true;                                \
    }                                             \
    if ((ctx).state() != tempState) return false; \
  }

// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TRY_LEXN(ctx, fn, ...)                    \
  {                                               \
    const auto tempState = (ctx).state();         \
    if (fn((ctx), __VA_ARGS__)) {                 \
      return true;                                \
    }                                             \
    if ((ctx).state() != tempState) return false; \
  }

namespace binary_op {

constexpr char MaxAsciiChar = numeric_limits<char>::max();

constexpr ValidSymbols BinaryOps = make_valid_symbols(
    {TokenType::BinaryOp10Left, TokenType::BinaryOp10Right,
     TokenType::BinaryOp9, TokenType::BinaryOp8, TokenType::BinaryOp7,
     TokenType::BinaryOp6, TokenType::BinaryOp5, TokenType::BinaryOp2,
     TokenType::BinaryOp1, TokenType::BinaryOp0});

constexpr array<char, 19> Chars{// OP10
                                /* 0 */ '$', '^',
                                // OP9
                                /* 2 */ '*', '%', '\\', '/',
                                // OP8
                                /* 6 */ '+', '-', '~', '|',
                                // OP7
                                /* 10 */ '&',
                                // OP6
                                /* 11 */ '.',
                                // OP5
                                /* 12 */ '=', '<', '>', '!',
                                // OP2
                                /* 16 */ '@', ':', '?'};

constexpr array<int8_t, 8> CharRanges{
    /* BinaryOpStart | BinaryOp10Left */ 0,
    /* BinaryOp10Right */ 1,
    /* BinaryOp9 */ 2,
    /* BinaryOp8 */ 6,
    /* BinaryOp7 */ 10,
    /* BinaryOp6 */ 11,
    /* BinaryOp5 */ 12,
    /* BinaryOp2 */ 16};

constexpr array<char16_t, 21> UnicodeChars{
    // OP9
    /* 0 */ u'∙', u'∘', u'×', u'★', u'⊗', u'⊘', u'⊙', u'⊛', u'⊠', u'⊡', u'∩',
    u'∧', u'⊓',
    // OP8
    /* 13 */ u'±', u'⊕', u'⊖', u'⊞', u'⊟', u'∪', u'∨', u'⊔'};

constexpr array<int8_t, 2> UnicodeRanges{
    /* BinaryOp9 */ 0,
    /* BinaryOp8 */ 13};

template<class InputIt, class MappingIt, class T>
int mapped_find(
    InputIt start, InputIt end, MappingIt mstart, MappingIt mend, T value,
    int fallback = -1)
{
  const auto iter = find(start, end, value);
  if (iter == end) {
    return fallback;
  }
  const auto idx = iter - start;
  const auto mapped_iter =
      find_if(mstart, mend, [=](auto rangeStart) { return rangeStart >= idx; });
  if (mapped_iter == mend) {
    return fallback;
  }

  return mapped_iter - mstart;
}

TokenType classify(char32_t character)
{
  if (character <= MaxAsciiChar) {
    if (const auto idx = mapped_find(
                             Chars.begin(), Chars.end(), CharRanges.begin(),
                             CharRanges.end(), character) != -1) {
      return (TokenType)idx;
    }

    return TokenType::TokenTypeLen;
  }

  if (const auto idx =
          mapped_find(
              UnicodeChars.begin(), UnicodeChars.end(), UnicodeRanges.begin(),
              UnicodeRanges.end(), character) != -1) {
    return (TokenType)idx;
  }

  return TokenType::TokenTypeLen;
}

bool is_op_char(char32_t character)
{
  if (character <= MaxAsciiChar) {
    return find(Chars.begin(), Chars.end(), character) != Chars.end();
  }

  return find(UnicodeChars.begin(), UnicodeChars.end(), character) !=
         UnicodeChars.end();
}

}  // namespace binary_op
}  // namespace

extern "C" {
void* tree_sitter_nim_external_scanner_create() noexcept { return nullptr; }

void tree_sitter_nim_external_scanner_destroy(void* payload) noexcept {}

unsigned tree_sitter_nim_external_scanner_serialize(
    void* /*payload*/, char* /*buffer*/) noexcept
{
  return 0;
}

void tree_sitter_nim_external_scanner_deserialize(
    void* /*payload*/, const char* /*buffer*/, unsigned /*length*/) noexcept
{
}

bool tree_sitter_nim_external_scanner_scan(
    void* /*payload*/, TSLexer* /*lexer*/,
    const bool* /*valid_symbols*/) noexcept
{
  return false;
}
}
