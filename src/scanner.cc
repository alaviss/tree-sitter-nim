/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#include <algorithm>
#include <array>
#include <bitset>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <locale>
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
constexpr auto BitsInByte = 8;

enum class TokenType : TSSymbol {
  TokenTypeStart,
  Comment = TokenTypeStart,
  StringContent,
  LongStringContent,
  LayoutStart,
  LayoutEnd,
  InvalidLayout,
  Line,
  LineElif,
  LineElse,
  LineExcept,
  LineFinally,
  LineOf,
  LineDo,
  BinaryOpStart,
  BinaryOp10Left = BinaryOpStart,
  BinaryOp10Right,
  BinaryOp9,
  BinaryOp8,
  BinaryOp7,
  BinaryOp6,
  BinaryOp5,
  BinaryOp2,
  SigilOp,
  BinaryOp1,
  BinaryOp0,
  UnaryOp,
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

enum class Flag { NotLineStart, FlagLen };

struct State {
  vector<uint8_t> layout_stack;
  uint8_t line_indent;
  bitset<(size_t)Flag::FlagLen> flags;

  unsigned serialize(uint8_t* buffer)
  {
    // Since spans doesn't exist before C++20, we have to do pointer math here.
    // NOLINTBEGIN(*-pointer-arithmetic)
    unsigned length = 0;
    constexpr unsigned max_length = TREE_SITTER_SERIALIZATION_BUFFER_SIZE;
    buffer[length++] = line_indent;
    static_assert(
        (int)Flag::FlagLen <= BitsInByte, "Flag set is bigger than expected!");
    buffer[length++] = (uint8_t)flags.to_ulong();
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
    flags = buffer[cursor++];
    const auto stack_size = length - cursor;
    layout_stack.resize(stack_size);
    for (unsigned i = 0; i < stack_size; i++) {
      layout_stack.at(i) = buffer[cursor++];
    }
    // NOLINTEND(*-pointer-arithmetic)
  }

  void set_flag(Flag flag) { flags.set(static_cast<size_t>(flag)); }

  void reset_flag(Flag flag) { flags.reset(static_cast<size_t>(flag)); }

  bool test_flag(Flag flag) const
  {
    return flags.test(static_cast<size_t>(flag));
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
  stream << ", flags: " << state.flags;
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

#ifdef TREE_SITTER_INTERNAL_BUILD
    if (getenv("TREE_SITTER_DEBUG")) {
      cerr << "lex_nim: valid symbols: " << valid_ << '\n';
    }
#endif
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
    counter_ += (int)!eof();
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

/// The maximum value of `char`. Useful for unicode testing.
constexpr char MaxAsciiChar = 0x7f;

bool is_lower(char32_t chr) { return chr >= 'a' && chr <= 'z'; }

bool is_alpha(char32_t chr)
{
  return (chr >= 'A' && chr <= 'Z') || is_lower(chr);
}

bool is_digit(char32_t chr) { return chr >= '0' && chr <= '9'; }

bool is_keyword_character(char32_t chr)
{
  return is_alpha(chr) || (chr == '_');
}

bool is_identifier_character(char32_t chr)
{
  return is_keyword_character(chr) || is_digit(chr) || (chr >= MaxAsciiChar);
}

char32_t to_lower(char32_t chr)
{
  constexpr char32_t ascii_lower_case_bit = 0x20;
  return is_alpha(chr) ? chr | ascii_lower_case_bit : chr;
}

string get_keyword(Context& ctx, int limit = -1)
{
  string keyword;
  int size = 0;
  if (is_lower(ctx.lookahead())) {
    keyword.push_back(static_cast<char>(ctx.lookahead()));
    size++;
    ctx.advance();
    while (is_keyword_character(ctx.lookahead()) &&
           (limit < 0 || size <= limit)) {
      if (ctx.lookahead() != '_') {
        keyword.push_back(static_cast<char>(to_lower(ctx.lookahead())));
        size++;
      }
      ctx.advance();
    }
    // If there are non-keyword characters in the identifier, then this is not
    // a keyword
    if (is_identifier_character(ctx.lookahead())) {
      keyword.clear();
    }
  }

  return keyword;
}

namespace operators {

/// The set of all BinaryOps tokens.
constexpr ValidSymbols BinaryOps = make_valid_symbols(
    {TokenType::BinaryOp10Left, TokenType::BinaryOp10Right,
     TokenType::BinaryOp9, TokenType::BinaryOp8, TokenType::BinaryOp7,
     TokenType::BinaryOp6, TokenType::BinaryOp5, TokenType::BinaryOp2,
     TokenType::BinaryOp1, TokenType::BinaryOp0});
constexpr ValidSymbols UnaryOps =
    make_valid_symbols({TokenType::UnaryOp, TokenType::SigilOp});

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
                                /* 16 */ ':', '?', '@'};

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

TokenType scan_operator(Context& ctx, bool immediate)
{
  enum class State {
    Arrow,
    Assignment,
    Colon,
    ColonColon,
    Dot,
    Equal,
    MaybeArrow,
    Minus,
    Regular,
  };

  auto state{State::Regular};
  const auto first_character = ctx.lookahead();
  auto result = classify(first_character);
#ifdef TREE_SITTER_INTERNAL_BUILD
  if (getenv("TREE_SITTER_DEBUG")) {
    cerr << "lex_nim: operator first symbol classification: " << (int)result
         << '\n';
  }
#endif
  if (result == TokenType::TokenTypeLen) {
    return result;
  }

  switch (first_character) {
  case '.':
    ctx.advance();
    state = State::Dot;
    break;
  case '=':
    ctx.advance();
    state = State::Equal;
    break;
  case ':':
    ctx.advance();
    state = State::Colon;
    break;
  case '-':
    ctx.advance();
    state = State::Minus;
    break;
  default:
    state = State::Regular;
    break;
  }

  while (is_op_char(ctx.lookahead())) {
    switch (state) {
    case State::Assignment:
    case State::Equal:
    case State::MaybeArrow:
    case State::Minus:
      switch (ctx.lookahead()) {
      case '>':
        state = State::Arrow;
        ctx.advance();
        break;
      default:
        state = State::Regular;
      }
      break;
    case State::Arrow:
    case State::Colon:
    case State::ColonColon:
    case State::Dot:
    case State::Regular:
      switch (ctx.lookahead()) {
      case '~':
      case '-':
        state = State::MaybeArrow;
        break;
      case '=':
        state = State::Assignment;
        break;
      case ':':
        state = state == State::Colon ? State::ColonColon : State::Regular;
        break;
      default:
        state = State::Regular;
        break;
      }
      ctx.advance();
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
  case State::Colon:
  case State::ColonColon:
  case State::Dot:
    return TokenType::TokenTypeLen;
  case State::Minus:
    if (is_digit(ctx.lookahead())) {
      return TokenType::TokenTypeLen;
    }
    break;
  default:
    break;
  }

  if (immediate) {
    return result;
  }

  switch (ctx.lookahead()) {
  case ' ':
  case '\n':
  case '\r':
    break;
  default:
    result = TokenType::UnaryOp;
  }

  return result;
}

/// Lexer for binary operators.
///
/// @param ctx - The context to scan from.
/// @param immediate - Whether the lexer was called before any spaces were
/// scanned.
bool lex(Context& ctx, bool immediate)
{
  if (!ctx.all_valid(BinaryOps) && !ctx.any_valid(UnaryOps)) {
    return false;
  }

  bool is_sigil = ctx.lookahead() == '@';
  auto result = scan_operator(ctx, immediate);

  if (result == TokenType::TokenTypeLen) {
    return false;
  }

  if (!ctx.all_valid(BinaryOps) || result == TokenType::UnaryOp) {
    result = is_sigil ? TokenType::SigilOp : TokenType::UnaryOp;
  }

#ifdef TREE_SITTER_INTERNAL_BUILD
  if (getenv("TREE_SITTER_DEBUG")) {
    cerr << "lex_nim: operator final classification: " << (int)result << '\n';
    cerr << "lex_nim: binary valid: " << ctx.all_valid(BinaryOps) << '\n';
    cerr << "lex_nim: unary valid: " << ctx.any_valid(UnaryOps) << '\n';
  }
#endif

  if (!ctx.valid(result)) {
    return false;
  }

  ctx.mark_end();
  return ctx.finish(result);
}

}  // namespace operators

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
      ctx.consume();
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
#ifdef TREE_SITTER_INTERNAL_BUILD
      if (getenv("TREE_SITTER_DEBUG")) {
        cerr << "lex_nim: block comment nest level: " << nesting << '\n';
      }
#endif
      break;
    default:
      break;
    }
    const auto want_end = nesting > 0 || !long_doc ? Marker::BlockComment
                                                   : Marker::BlockDocComment;

    if (consume_end_marker(ctx, want_end)) {
#ifdef TREE_SITTER_INTERNAL_BUILD
      if (getenv("TREE_SITTER_DEBUG")) {
        cerr << "lex_nim: block comment terminate nest level: " << nesting
             << '\n';
      }
#endif
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

namespace line {
constexpr auto LineTokens = make_valid_symbols(
    {TokenType::Line, TokenType::LineElif, TokenType::LineElse,
     TokenType::LineOf, TokenType::LineExcept, TokenType::LineFinally,
     TokenType::LineDo});
constexpr auto LineKeyword = make_valid_symbols(
    {TokenType::LineElif, TokenType::LineElse, TokenType::LineOf,
     TokenType::LineExcept, TokenType::LineFinally, TokenType::LineDo});

bool lex(Context& ctx)
{
  if (!ctx.any_valid(LineTokens)) {
    return false;
  }
  ctx.mark_end();

  if (ctx.any_valid(LineKeyword)) {
    constexpr auto longest_word_length = 7; /* finally */
    auto keyword = get_keyword(ctx, longest_word_length);

#ifdef TREE_SITTER_INTERNAL_BUILD
    if (getenv("TREE_SITTER_DEBUG")) {
      cerr << "lex_nim: keyword after line: " << keyword << '\n';
    }
#endif

    if (keyword == "elif" && ctx.valid(TokenType::LineElif)) {
      return ctx.finish(TokenType::LineElif);
    }
    if (keyword == "else" && ctx.valid(TokenType::LineElse)) {
      return ctx.finish(TokenType::LineElse);
    }
    if (keyword == "of" && ctx.valid(TokenType::LineOf)) {
      return ctx.finish(TokenType::LineOf);
    }
    if (keyword == "except" && ctx.valid(TokenType::LineExcept)) {
      return ctx.finish(TokenType::LineExcept);
    }
    if (keyword == "finally" && ctx.valid(TokenType::LineFinally)) {
      return ctx.finish(TokenType::LineFinally);
    }
    if (keyword == "do" && ctx.valid(TokenType::LineDo)) {
      return ctx.finish(TokenType::LineDo);
    }
  }

  if (ctx.valid(TokenType::Line) && !ctx.eof()) {
    return ctx.finish(TokenType::Line);
  }

  return false;
}
}  // namespace line

namespace string_lex {
constexpr auto StringTokens = make_valid_symbols(
    {TokenType::StringContent, TokenType::LongStringContent});

bool handle_long_string_quote(Context& ctx)
{
  if (ctx.lookahead() != '"' || !ctx.valid(TokenType::LongStringContent)) {
    return false;
  }

  ctx.consume();
  uint8_t count = 1;
  while (ctx.lookahead() == '"' && count < 3) {
    ctx.advance();
    count++;
  }

  if (count < 3 || ctx.lookahead() == '"') {
    return ctx.finish(TokenType::LongStringContent);
  }

  return false;
}

bool lex(Context& ctx)
{
  if (!ctx.any_valid(StringTokens) || ctx.error()) {
    return false;
  }

  TRY_LEX(ctx, handle_long_string_quote);
  bool has_content = false;
  // NOLINTBEGIN(*-avoid-goto)
  while (!ctx.eof()) {
    switch (ctx.lookahead()) {
    case '\n':
    case '\r':
    case '\\':
      if (!ctx.valid(TokenType::LongStringContent)) {
        goto loop_break;
      }
      break;
    case '"':
      goto loop_break;
    }

    has_content = true;
    ctx.consume();
  }
  // NOLINTEND(*-avoid-goto)

loop_break:
  return has_content && ctx.finish(
                            ctx.valid(TokenType::LongStringContent)
                                ? TokenType::LongStringContent
                                : TokenType::StringContent);
}
}  // namespace string_lex

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
    // Don't open new root on error
    if (ctx.state().layout_stack.empty() && ctx.error()) {
      return false;
    }
    ctx.state().layout_stack.push_back(line_indent);
    ctx.mark_end();
    return ctx.finish(TokenType::LayoutStart);
  }

  if (ctx.valid(TokenType::LayoutEnd) && !ctx.state().layout_stack.empty()) {
    if (last_indent > line_indent) {
      ctx.state().layout_stack.pop_back();
      ctx.mark_end();
      return ctx.finish(TokenType::LayoutEnd);
    }

    if (ctx.eof()) {
      ctx.state().layout_stack.pop_back();
      ctx.mark_end();
      return ctx.finish(TokenType::LayoutEnd);
    }
  }

  if (last_indent > line_indent) {
    ctx.mark_end();
    return ctx.finish(TokenType::InvalidLayout);
  }

  if (!ctx.state().test_flag(Flag::NotLineStart)) {
    ctx.state().set_flag(Flag::NotLineStart);
    TRY_LEX(ctx, line::lex);
  }

  return false;
}

void lex_space(Context& ctx)
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
    ctx.state().reset_flag(Flag::NotLineStart);
  }
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

  TRY_LEX(ctx, string_lex::lex);
  TRY_LEXN(ctx, operators::lex, true);

  lex_space(ctx);

  TRY_LEX(ctx, comment::lex);
  TRY_LEX(ctx, lex_indent);
  TRY_LEXN(ctx, operators::lex, false);

  return false;
}
}
