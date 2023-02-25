/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#include <algorithm>
#include <array>
#include <bitset>
#include <cstdint>
#include <limits>
#include <ostream>
#include <utility>
#include <vector>

#ifdef TREE_SITTER_INTERNAL_BUILD
#  include <iostream>
#endif

#include "tree_sitter/parser.h"

namespace {

using namespace std;

using IndentCount = uint8_t;
constexpr auto InvalidIndent = numeric_limits<IndentCount>::max();

enum class TokenType : TSSymbol {
  TokenTypeStart,
  Comment = TokenTypeStart,
  LongStringQuote,
  LayoutStart,
  LayoutEnd,
  InvalidLayout,
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

struct State {
  vector<uint8_t> layout_stack;
  uint8_t line_indent;

  unsigned serialize(uint8_t* buffer)
  {
    // Since spans doesn't exist before C++20, we have to do pointer math here.
    // NOLINTBEGIN(*-pointer-arithmetic)
    unsigned length = 0;
    constexpr unsigned max_length = TREE_SITTER_SERIALIZATION_BUFFER_SIZE;
    buffer[length++] = line_indent;
    for (const auto indent : layout_stack) {
      // Terrible failure mode, but there isn't much to do here.
      if (length >= max_length) {
        break;
      }
      buffer[length++] = indent;
    }
    return length;
    // NOLINTEND(*-pointer-arithmetic)
  }

  void deserialize(const uint8_t* buffer, unsigned length)
  {
    // NOLINTBEGIN(*-pointer-arithmetic)
    line_indent = 0;
    layout_stack.clear();

    if (length == 0) {
      return;
    }

    unsigned cursor = 0;
    line_indent = buffer[cursor++];
    const auto stack_size = length - cursor;
    layout_stack.resize(stack_size);
    for (unsigned i = 0; i < stack_size; i++) {
      layout_stack.at(i) = buffer[cursor++];
    }
    // NOLINTEND(*-pointer-arithmetic)
  }
};

#ifdef TREE_SITTER_INTERNAL_BUILD
ostream& operator<<(ostream& stream, const State& state)
{
  stream << "current indentation: " << (int32_t)state.line_indent;
  stream << ", indentation stack: [ ";
  for (const auto indent : state.layout_stack) {
    stream << (int32_t)indent << " ";
  }
  stream << "]";
  return stream;
}
#endif

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
  Context(TSLexer* lexer, State* state, const bool* validsyms) :
      lexer_(lexer), state_(state)
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
    return (valid_ & sym) == sym;
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
    counter_ += (int)eof();
    lexer_->advance(lexer_, skip);
    return lookahead();
  }

  /// Returns the advance counter.
  ///
  /// This count changes every time the lexer is advanced.
  uint32_t counter() const { return counter_; }

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
#ifdef TREE_SITTER_INTERNAL_BUILD
    if (getenv("TREE_SITTER_DEBUG")) {
      cerr << "lex_nim: finish state: " << *state_ << '\n';
    }
#endif
    lexer_->result_symbol = (TSSymbol)type;
    return true;
  }

  /// Access the persistent state.
  State& state() { return *state_; }

private:
  TSLexer* lexer_;
  State* state_;
  uint32_t counter_{};
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
#define TRY_LEX(ctx, fn)                             \
  do {                                               \
    const auto last_count = (ctx).counter();         \
    if (fn((ctx))) {                                 \
      return true;                                   \
    }                                                \
    if ((ctx).counter() != last_count) return false; \
  } while (false)

/// Same as {@link TRY_LEX}, but allows passing extra arguments to the lexing
/// function.
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define TRY_LEXN(ctx, fn, ...)                       \
  do {                                               \
    const auto last_count = (ctx).counter();         \
    if (fn((ctx), __VA_ARGS__)) {                    \
      return true;                                   \
    }                                                \
    if ((ctx).counter() != last_count) return false; \
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

/// Given a segmented array, returns the segment the given value is in.
///
/// @param array - The segmented array.
/// @param mapping - The start of each segment, sorted from low to high.
/// @param value - The value to search for.
/// @param fallback - The value returned if `value` is not in `array`, that the
/// segment found is not in `mapping`.
///
/// @returns The index of the segment in `mapping`.
template<class InputT, class MapT, class T>
int mapped_find(
    const InputT& array, const MapT& mapping, T value, int fallback = -1)
{
  const auto iter = find(array.begin(), array.end(), value);
  if (iter == array.end()) {
    return fallback;
  }
  const auto idx = iter - array.begin();
  const auto mapped_iter = find_if(
      mapping.rbegin(), mapping.rend(),
      [=](auto rangeStart) { return rangeStart <= idx; });
  if (mapped_iter == mapping.rend()) {
    return fallback;
  }

  return mapping.rend() - mapped_iter - 1;
}

/// Returns the BinaryOp token type corresponding to the given `character`.
///
/// Returns `TokenType::TokenTypeLen` if the character is not a valid operator.
TokenType classify(char32_t character)
{
  if (character <= MaxAsciiChar) {
    const auto idx = mapped_find(Chars, CharRanges, character);
    if (idx != -1) {
      return (TokenType)(idx + (uint8_t)TokenType::BinaryOpStart);
    }

    return TokenType::TokenTypeLen;
  }

  const auto idx = mapped_find(UnicodeChars, UnicodeRanges, character);
  if (idx != -1) {
    return (TokenType)(idx + (uint8_t)TokenType::BinaryOp9);
  }

  return TokenType::TokenTypeLen;
}

/// Returns whether `character` is a valid operator character.
bool is_op_char(char32_t character)
{
  if (character <= MaxAsciiChar) {
    return find(Chars.begin(), Chars.end(), character) != Chars.end();
  }

  return find(UnicodeChars.begin(), UnicodeChars.end(), character) !=
         UnicodeChars.end();
}

/// Lexer for binary operators.
///
/// @param ctx - The context to scan from.
/// @param immediate - Whether the lexer was called before any spaces were
/// scanned.
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
    Regular,
  };

  auto state{State::Regular};
  const auto first_character = ctx.lookahead();
  auto result = classify(first_character);
  if (result == TokenType::TokenTypeLen) {
    return false;
  }

  switch (first_character) {
  case '.':
    ctx.consume();
    state = State::MaybeDotOp;
    break;
  case '=':
    ctx.consume();
    state = State::Equal;
    break;
  case ':':
    ctx.consume();
    state = State::Colon;
    break;
  default:
    state = State::Regular;
    break;
  }

  while (is_op_char(ctx.lookahead())) {
    switch (state) {
    case State::MaybeDotOp:
      switch (ctx.lookahead()) {
      case '.':
        state = State::Regular;
        ctx.consume();
        break;
      default:
        return false;
      }
      break;
    case State::Assignment:
    case State::Equal:
    case State::MaybeArrow:
      switch (ctx.lookahead()) {
      case '>':
        state = State::Arrow;
        ctx.consume();
        break;
      default:
        state = State::Regular;
      }
      break;
    case State::Arrow:
    case State::Colon:
    case State::Regular:
      switch (ctx.lookahead()) {
      case '~':
      case '-':
        state = State::MaybeArrow;
        break;
      case '=':
        state = State::Assignment;
        break;
      default:
        state = State::Regular;
        break;
      }
      ctx.consume();
      break;
    }
  }

  switch (state) {
  case State::Arrow:
    result = TokenType::BinaryOp0;
    break;
  case State::Assignment:
    switch (first_character) {
    case '<':
    case '>':
    case '!':
    case '=':
    case '~':
    case '?':
      break;
    default:
      result = TokenType::BinaryOp1;
    }
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
    const auto last_state = ctx.counter();

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

    if (last_state == ctx.counter()) {
      ctx.consume();
    }
  }

  return false;
}
}  // namespace comment

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

bool lex_indent(Context& ctx)
{
  if (ctx.lookahead() == '#') {
    return false;
  }

  const auto line_indent = ctx.state().line_indent;

  if (line_indent == InvalidIndent) {
#ifdef TREE_SITTER_INTERNAL_BUILD
    if (getenv("TREE_SITTER_DEBUG")) {
      cerr << "lex_nim: invalid indentation reached\n";
    }
#endif
    return false;
  }

  const int32_t last_indent =
      !ctx.state().layout_stack.empty() ? ctx.state().layout_stack.back() : -1;

  if (ctx.valid(TokenType::LayoutStart) && last_indent < line_indent) {
    ctx.state().layout_stack.push_back(line_indent);
    ctx.mark_end();
    return ctx.finish(TokenType::LayoutStart);
  }

  if (ctx.valid(TokenType::LayoutEnd)) {
    if (last_indent > line_indent) {
      ctx.state().layout_stack.pop_back();
      ctx.mark_end();
      return ctx.finish(TokenType::LayoutEnd);
    }

    if (ctx.eof()) {
      ctx.mark_end();
      return ctx.finish(TokenType::LayoutEnd);
    }
  }

  if (last_indent > line_indent) {
    ctx.mark_end();
    return ctx.finish(TokenType::InvalidLayout);
  }

  return false;
}

bool lex_space_and_terminator(Context& ctx)
{
  bool found = false;
  uint8_t indent = 0;
  while (true) {
    switch (ctx.lookahead()) {
    case ' ':
      if (found || ctx.state().layout_stack.empty()) {
        indent += (int)(indent != InvalidIndent);
      }
      ctx.advance(false);
      break;
    case '\0':
      if (ctx.eof()) {
        found = true;
        indent = 0;
        ctx.mark_end();
      }
      goto end;  // NOLINT(*-avoid-goto)
      break;
    case '\n':
    case '\r':
      found = true;
      indent = 0;
      ctx.consume();
      break;
    default:
      goto end;  // NOLINT(*-avoid-goto)
    }
  }
end:
  if (found) {
    ctx.state().line_indent = indent;

    if (ctx.valid(TokenType::Terminator)) {
      return ctx.finish(TokenType::Terminator);
    }
  }

  return false;
}

}  // namespace

extern "C" {
void* tree_sitter_nim_external_scanner_create() noexcept { return new State{}; }

void tree_sitter_nim_external_scanner_destroy(void* payload) noexcept
{
  auto* state = static_cast<State*>(payload);
  // We own this and the code does not do anything to justify pulling
  // unique_ptr in.
  //
  // NOLINTNEXTLINE(*-owning-memory)
  delete state;
}

unsigned tree_sitter_nim_external_scanner_serialize(
    void* payload, uint8_t* buffer) noexcept
{
  auto* state = static_cast<State*>(payload);
  return state->serialize(buffer);
}

void tree_sitter_nim_external_scanner_deserialize(
    void* payload, const uint8_t* buffer, unsigned length) noexcept
{
  auto* state = static_cast<State*>(payload);
  state->deserialize(buffer, length);
}

bool tree_sitter_nim_external_scanner_scan(
    void* payload, TSLexer* lexer, const bool* valid_symbols) noexcept
{
  Context ctx{lexer, static_cast<State*>(payload), valid_symbols};

#ifdef TREE_SITTER_INTERNAL_BUILD
  if (getenv("TREE_SITTER_DEBUG")) {
    cerr << "lex_nim: start state: " << ctx.state() << '\n';
  }
#endif

  TRY_LEX(ctx, lex_long_string_quote);
  TRY_LEXN(ctx, binary_op::lex, true);

  if (lex_space_and_terminator(ctx)) {
    return true;
  }

  TRY_LEX(ctx, comment::lex);
  TRY_LEX(ctx, lex_indent);
  TRY_LEXN(ctx, binary_op::lex, false);

  return false;
}
}
