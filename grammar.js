/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

/// <reference types="tree-sitter-cli/dsl" />

const DecimalLiteral = sep1(/[0-9]+/, "_");
const Identifier = seq(
  /[a-zA-Z\x80-\xff]+/,
  repeat(/[a-zA-Z\x80-\xff0-9]+/),
  repeat(seq("_", /[a-zA-Z\x80-\xff0-9]+/))
);

module.exports = grammar({
  name: "nim",

  word: $ => $.identifier,
  externals: $ => [$._long_string_quote, $._terminator],

  rules: {
    source_file: $ => repeat(seq($._literals, $._terminator)),

    _literals: $ =>
      choice(
        $.boolean_literal,
        $.char_literal,
        $.custom_numeric_literal,
        $.float_literal,
        $.generalized_string_literal,
        $.integer_literal,
        $.nil_literal,
        $.string_literal
      ),

    boolean_literal: _ =>
      token(
        choice(
          ignoreStyle("true"),
          ignoreStyle("false"),
          ignoreStyle("on"),
          ignoreStyle("off")
        )
      ),

    char_literal: $ =>
      seq(
        "'",
        choice(
          token.immediate(/[^\x00-\x1f\\]/),
          $._backslash_literal,
          alias($._char_escape_sequence, $.escape_sequence)
        ),
        token.immediate("'")
      ),

    _backslash_literal: _ => token.immediate("\\\\"),

    _char_escape_sequence: _ =>
      token.immediate(
        seq("\\", choice(/[rcnlftv\\"'abe]/, /\d+/, /x[0-9a-fA-F]{2}/))
      ),

    integer_literal: $ =>
      seq(
        $._numeric_literal,
        optional(token.immediate("'")),
        optional(token.immediate(choice(/[uU]/, /[iIuU](8|16|32|64)/)))
      ),

    float_literal: $ => {
      const FloatSuffix = /[fFdD](32|64|128)?/;
      const Apostrophe = optional(token.immediate("'"));

      return choice(
        seq($._numeric_literal, Apostrophe, token.immediate(FloatSuffix)),
        seq(
          $._decimal_float_literal,
          Apostrophe,
          optional(token.immediate(FloatSuffix))
        )
      );
    },

    custom_numeric_literal: $ =>
      seq(
        choice($._numeric_literal, $._decimal_float_literal),
        field("function", seq(token.immediate("'"), $._identifier_imm))
      ),

    _numeric_literal: _ =>
      token(
        seq(
          optional("-"),
          choice(
            DecimalLiteral,
            seq(/0[xX]/, sep1(/[0-9a-fA-F]+/, "_")),
            seq("0o", sep1(/[0-7]+/, "_")),
            seq(/0[bB]/, sep1(/[01]+/, "_"))
          )
        )
      ),

    _decimal_float_literal: _ =>
      token(
        seq(
          optional("-"),
          DecimalLiteral,
          seqReq1(seq(".", DecimalLiteral), seq(/[eE][+-]?/, DecimalLiteral))
        )
      ),

    nil_literal: _ => ignoreStyle("nil"),

    string_literal: $ =>
      choice(
        $._long_string_literal,
        $._interpreted_string_literal,
        $._raw_string_literal
      ),

    _interpreted_string_literal: $ =>
      seq(
        '"',
        repeat(
          choice(
            token.immediate(/[^\n\r"\\]+/),
            alias($._string_escape_sequence, $.escape_sequence)
          )
        ),
        token.immediate('"')
      ),

    _string_escape_sequence: $ =>
      choice(
        $._char_escape_sequence,
        token.immediate("\\p"),
        token.immediate(
          seq("\\u", choice(/[0-9a-fA-F]{4}/, /\{[0-9a-fA-F]+\}/))
        )
      ),

    _raw_string_content: _ =>
      token.immediate(
        seq(
          /[^\n\r"]+/,
          repeat(seq('""', optional(token.immediate(/[^\n\r"]+/))))
        )
      ),

    _raw_string_literal: $ =>
      seq('r"', optional($._raw_string_content), token.immediate('"')),

    _long_string_literal: $ =>
      seq(/r?"""/, optional($._long_string_content), token.immediate('"""')),

    _long_string_content: $ =>
      repeat1(
        choice(token.immediate(/[^"]+/), alias($._long_string_quote, '"'))
      ),

    generalized_string_literal: $ =>
      choice($._generalized_short_string, $._generalized_long_string),

    _generalized_short_string: $ =>
      seq(
        field("function", $.identifier),
        token.immediate('"'),
        optional($._raw_string_content),
        token.immediate('"')
      ),

    _generalized_long_string: $ =>
      seq(
        field("function", $.identifier),
        token.immediate('"""'),
        optional($._long_string_content),
        token.immediate('"""')
      ),

    _identifier_imm: $ => alias(token.immediate(Identifier), $.identifier),
    identifier: _ => token(Identifier),
  },
});

/**
 * Produce a rule that matches one or more occurance of a given rule.
 * Each of the rules are separated by the given separator.
 * @param {RuleOrLiteral} rule - The rule to be matched.
 * @param {RuleOrLiteral} sep - The matched separator.
 */
function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}

/**
 * Produce a rule to match nim-style style insensitive identifiers
 *
 * @param {string} ident - The string to match style-insensitively.
 */
function ignoreStyle(ident) {
  /** @type {RuleOrLiteral[]} */
  let rules = [ident[0]];
  for (let i = 1, L = ident.length; i < L; i++) {
    const lower = ident[i].toLowerCase();
    const upper = ident[i].toUpperCase();

    rules.push(optional("_"), choice(lower, upper));
  }

  return token(seq(...rules));
}

/**
 * Produce a rule where the given rules matches in order, but only one of them has to match.
 *
 * @param {RuleOrLiteral[]} rules - The rules to be matched.
 */
function seqReq1(...rules) {
  /** @type {Rule[]} */
  let result = [];

  while (rules.length > 0) {
    result.push(seq(rules.shift(), ...rules.map(x => optional(x))));
  }

  return choice(...result);
}
