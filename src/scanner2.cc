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

using ValidSymbols = bitset<(size_t)TokenType::TokenTypeLen>;

constexpr ValidSymbols make_valid_symbols(initializer_list<TokenType> syms)
{
  uint64_t result{};
  for (const auto sym : syms) {
    result |= 1U << uint8_t(sym);
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

  /// Returns whether any of the given tokens in `sym` are valid at the current
  /// position.
  [[nodiscard]] bool any_valid(ValidSymbols sym) const
  {
    return (valid_ & sym).any();
  }

  /// Returns whether all of the given tokens in `sym` are valid at the current
  /// position.
  [[nodiscard]] bool all_valid(ValidSymbols sym) const
  {
    return (valid_ & sym).all();
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
  ValidSymbols valid_{};
};

/// Try lexing with the given function.
///
/// If the function succeed, the lexer returns immediately.
/// Otherwise, if no input were consumed, the lexer will continue.
///
/// The lexer will stop immediately if input was consumed and the given lexing
/// function fails.
///
/// @param ctx - The context to monitor state with, and as input to `fn`.
/// @param fn - The lexing function
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TRY_LEX(ctx, fn)                           \
  do {                                             \
    const auto last_state = (ctx).state();         \
    if (fn((ctx))) {                               \
      return true;                                 \
    }                                              \
    if ((ctx).state() != last_state) return false; \
  } while (false)

/// Same as {@link TRY_LEX}, but allows passing extra arguments to the lexing
/// function.
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TRY_LEXN(ctx, fn, ...)                     \
  do {                                             \
    const auto last_state = (ctx).state();         \
    if (fn((ctx), __VA_ARGS__)) {                  \
      return true;                                 \
    }                                              \
    if ((ctx).state() != last_state) return false; \
  } while (false)

namespace binary_op {

/// The maximum value of `char`. Useful for unicode testing.
constexpr char MaxAsciiChar = numeric_limits<char>::max();

/// The set of all BinaryOps tokens.
constexpr ValidSymbols BinaryOps = make_valid_symbols(
    {TokenType::BinaryOp10Left, TokenType::BinaryOp10Right,
     TokenType::BinaryOp9, TokenType::BinaryOp8, TokenType::BinaryOp7,
     TokenType::BinaryOp6, TokenType::BinaryOp5, TokenType::BinaryOp2,
     TokenType::BinaryOp1, TokenType::BinaryOp0});

// TODO: Invent something to encapsulate this structure.

/// All ASCII operator characters.
///
/// The ordering is done so that it reflects the precedence structure.
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

/// Mapping of {@link Chars} starting index and offset from BinaryOpsStart.
constexpr array<int8_t, 8> CharRanges{
    /* BinaryOpStart | BinaryOp10Left */ 0,
    /* BinaryOp10Right */ 1,
    /* BinaryOp9 */ 2,
    /* BinaryOp8 */ 6,
    /* BinaryOp7 */ 10,
    /* BinaryOp6 */ 11,
    /* BinaryOp5 */ 12,
    /* BinaryOp2 */ 16};

/// All Unicode operator characters.
///
/// The ordering is done so that it reflects the precedence structure.
constexpr array<char16_t, 21> UnicodeChars{
    // OP9
    /* 0 */ u'∙', u'∘', u'×', u'★', u'⊗', u'⊘', u'⊙', u'⊛', u'⊠', u'⊡', u'∩',
    u'∧', u'⊓',
    // OP8
    /* 13 */ u'±', u'⊕', u'⊖', u'⊞', u'⊟', u'∪', u'∨', u'⊔'};

/// Mapping of {@link UnicodeChars} starting index and offset from
/// BinaryOpsStart.
constexpr array<int8_t, 2> UnicodeRanges{
    /* BinaryOp9 */ 0,
    /* BinaryOp8 */ 13};

/// Find the given `value` within the range `start`..`end` and map it to the
/// range `mstart`..`mend`.
///
/// The mapping works by converting the result of the search in `start`..`end`
/// to an index, and the index of first value in `mstart`..`mend` that
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
    const auto idx = mapped_find(
        Chars.begin(), Chars.end(), CharRanges.begin(), CharRanges.end(),
        character);
    if (idx != -1) {
      return (TokenType)(idx + (uint8_t)TokenType::BinaryOpStart);
    }

    return TokenType::TokenTypeLen;
  }

  const auto idx = mapped_find(
      UnicodeChars.begin(), UnicodeChars.end(), UnicodeRanges.begin(),
      UnicodeRanges.end(), character);
  if (idx != -1) {
    return (TokenType)(idx + (uint8_t)TokenType::BinaryOp9);
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

bool lex(Context& ctx, bool immediate)
{
  if (!ctx.all_valid(BinaryOps)) {
    return false;
  }

  enum class State {
    Arrow,
    Assignment,
    Colon,
    Equal,
    MaybeArrow,
    MaybeDotOp,
    Scan,
    Start,
  };

  auto state{State::Start};
  const auto first_character = ctx.lookahead();

  while (is_op_char(ctx.lookahead())) {
    switch (state) {
    case State::Start:
      switch (ctx.lookahead()) {
      case '.':
        ctx.consume();
        state = State::MaybeDotOp;
        break;
      case '=':
        state = State::Equal;
        break;
      case ':':
        state = State::Colon;
        break;
      default:
        state = State::Scan;
        break;
      }
      break;
    case State::MaybeDotOp:
      if (ctx.lookahead() != '.') {
        return false;
      }
      state = State::Scan;
      break;
    case State::Arrow:
    case State::Colon:
    case State::Equal:
    case State::Assignment:
    case State::MaybeArrow:
    case State::Scan:
      switch (ctx.consume()) {
      case '~':
      case '-':
        state = State::MaybeArrow;
        break;
      case '=':
        state = State::Assignment;
        break;
      case '>':
        switch (state) {
        case State::MaybeArrow:
        case State::Assignment:
          state = State::Arrow;
          break;
        default:
          break;
        }
        break;
      default:
        state = State::Scan;
        break;
      }
      break;
    }
  }

  auto result = classify(first_character);
  switch (state) {
  case State::Arrow:
    result = TokenType::BinaryOp0;
    break;
  case State::Assignment:
    result = TokenType::BinaryOp1;
    break;
  case State::Equal:
    if (ctx.valid(TokenType::Equal)) {
      result = TokenType::Equal;
    }
    break;
  case State::Colon:
    if (ctx.valid(TokenType::Colon)) {
      result = TokenType::Colon;
    }
    break;
  case State::MaybeDotOp:
  case State::Start:
    return false;
  default:
    break;
  }

  if (immediate) {
    return ctx.finish(result);
  }

  switch (ctx.lookahead()) {
  case ' ':
  case '\n':
  case '\r':
    break;
  default:
    return false;
  }

  return ctx.finish(result);
}

}  // namespace binary_op

bool lex_long_string_quote(Context& ctx)
{
  if (!ctx.valid(TokenType::LongStringQuote) || ctx.lookahead() != '"') {
    return false;
  }

  ctx.consume();
  uint8_t count = 1;
  while (ctx.lookahead() == '"' && count < 3) {
    ctx.advance();
    count++;
  }

  if (count < 3 || ctx.lookahead() == '"') {
    return ctx.finish(TokenType::LongStringQuote);
  }

  return false;
}

bool lex_space_and_terminator(Context& ctx)
{
  bool found = false;
  while (true) {
    switch (ctx.lookahead()) {
    case ' ':
      ctx.advance(false);
      break;
    case '\0':
      if (ctx.eof()) {
        found = true;
        ctx.mark_end();
      }
      goto end;  // NOLINT(*-avoid-goto)
      break;
    case '\n':
    case '\r':
      found = true;
      ctx.consume();
      break;
    default:
      goto end;  // NOLINT(*-avoid-goto)
    }
  }

end:
  if (found && ctx.valid(TokenType::Terminator)) {
    return ctx.finish(TokenType::Terminator);
  }

  return false;
}

namespace comment {
enum class Marker { Invalid, LineComment, BlockComment, BlockDocComment };

Marker start_marker(Context& ctx)
{
  switch (ctx.lookahead()) {
  case '#':
    switch (ctx.consume()) {
    case '#':
      if (ctx.consume() == '[') {
        return Marker::BlockDocComment;
      }
      return Marker::LineComment;
    case '[':
      return Marker::BlockComment;
    }
    return Marker::LineComment;
  default:
    return Marker::Invalid;
  }
}

bool consume_end_marker(Context& ctx, Marker type)
{
  switch (type) {
  case Marker::BlockComment:
  case Marker::BlockDocComment:
    if (ctx.lookahead() != ']') {
      return false;
    }
    if (ctx.consume() != '#') {
      return false;
    }
    if (type == Marker::BlockComment) {
      return true;
    }
    if (ctx.consume() != '#') {
      return false;
    }
    ctx.consume();
    return true;
  default:
    return false;
  }
}

bool handle_line_comment(Context& ctx)
{
  while (!ctx.eof()) {
    switch (ctx.lookahead()) {
    case '\n':
    case '\r':
      return ctx.finish(TokenType::Comment);
    }
    ctx.consume();
  }
  return ctx.finish(TokenType::Comment);
}

bool lex(Context& ctx)
{
  if (!ctx.valid(TokenType::Comment)) {
    return false;
  }

  bool long_doc = false;
  switch (start_marker(ctx)) {
  case Marker::LineComment:
    return handle_line_comment(ctx);
  case Marker::BlockDocComment:
    long_doc = true;
    [[fallthrough]];
  case Marker::BlockComment:
    break;
  default:
    return false;
  }

  uint32_t nesting = 0;
  while (!ctx.eof()) {
    const auto last_state = ctx.state();

    switch (start_marker(ctx)) {
    case Marker::BlockComment:
    case Marker::BlockDocComment:
      nesting++;
      break;
    default:
      break;
    }
    const auto want_end = nesting > 0 || !long_doc ? Marker::BlockComment
                                                   : Marker::BlockDocComment;
    if (consume_end_marker(ctx, want_end)) {
      if (nesting == 0) {
        return ctx.finish(TokenType::Comment);
      }
      nesting--;
    }

    if (last_state == ctx.state()) {
      ctx.consume();
    }
  }

  return false;
}
}  // namespace comment
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
    void* /*payload*/, TSLexer* lexer, const bool* valid_symbols) noexcept
{
  Context ctx{lexer, valid_symbols};
  TRY_LEX(ctx, lex_long_string_quote);
  TRY_LEXN(ctx, binary_op::lex, true);

  if (lex_space_and_terminator(ctx)) {
    return true;
  }

  TRY_LEX(ctx, comment::lex);
  TRY_LEXN(ctx, binary_op::lex, false);

  return false;
}
}
