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
const UnicodeMul = [
  "∙",
  "∘",
  "×",
  "★",
  "⊗",
  "⊘",
  "⊙",
  "⊛",
  "⊠",
  "⊡",
  "∩",
  "∧",
  "⊓",
];
const UnicodeAdd = ["±", "⊕", "⊖", "⊞", "⊟", "∪", "∨", "⊔"];
const OperatorChars = [
  "=",
  "+",
  "-",
  "*",
  "/",
  "<",
  ">",
  "@",
  "$",
  "~",
  "&",
  "%",
  "|",
  "!",
  "?",
  "^",
  ".",
  ":",
  "\\",
  ...UnicodeMul,
  ...UnicodeAdd,
];

const Precedence = {
  ColonEqExpr: 15,
  Sigil: 14,
  Dot: 13,
  Suffix: 12,
  Unary: 11,
  Op10: 10,
  Op9: 9,
  Op8: 8,
  Op7: 7,
  Op6: 6,
  Op5: 5,
  Op4: 4,
  Op3: 3,
  Op2: 2,
  Op1: 1,
  Op0: 0,
};

const WordOp = {
  9: token(choice(...["div", "mod", "shl", "shr"].map(ignoreStyle))),
  5: token(
    choice(
      ...["not", "in", "notin", "is", "isnot", "of", "as", "from"].map(
        ignoreStyle
      )
    )
  ),
  4: ignoreStyle("and"),
  3: token(choice(...["or", "xor"].map(ignoreStyle))),
};

module.exports = grammar({
  name: "nim",

  word: $ => $.identifier,
  externals: $ => [
    $.comment,
    $._long_string_quote,
    $._terminator,
    ":",
    "=",
    $._binop10l,
    $._binop10r,
    $._binop9,
    $._binop8,
    $._binop7,
    $._binop6,
    $._binop5,
    $._binop2,
    $._binop1,
    $._binop0,
  ],
  extras: $ => [/[\n\r ]+/, $.comment],
  inline: $ => [$._maybe_colon_expression, $._maybe_equal_expression],

  rules: {
    source_file: $ => repeat(seq($._expression, $._terminator)),

    _expression: $ =>
      choice(
        $._literals,
        $.accent_quoted,
        $.binary_expression,
        $.bracket_expression,
        $.curly_expression,
        $.dot_expression,
        $.parenthesized_expression,
        $.tuple,
        $.unary_expression,
        $.identifier
      ),

    argument_list: $ =>
      sep1(choice($._maybe_colon_expression, $._maybe_equal_expression), ","),

    colon_expression: $ =>
      seq(field("left", $._expression), ":", field("right", $._expression)),
    equal_expression: $ =>
      seq(field("left", $._expression), "=", field("right", $._expression)),

    _maybe_colon_expression: $ => choice($._expression, $.colon_expression),
    _maybe_equal_expression: $ => choice($._expression, $.colon_expression),

    tuple: $ =>
      choice(
        seq(
          "(",
          $._expression,
          ",",
          optional(sep1($._maybe_colon_expression, ",")),
          ")"
        ),
        seq(
          "(",
          $.colon_expression,
          repeat(seq(",", $._maybe_colon_expression)),
          ")"
        )
      ),

    parenthesized_expression: $ => seq("(", $._expression, ")"),

    bracket_expression: $ =>
      prec.left(
        Precedence.Suffix,
        seq(
          field("left", $._expression),
          "[",
          field("right", $.argument_list),
          "]"
        )
      ),

    curly_expression: $ =>
      prec.left(
        Precedence.Suffix,
        seq(
          field("left", $._expression),
          "{",
          field("right", $.argument_list),
          "}"
        )
      ),

    dot_expression: $ =>
      prec.left(
        Precedence.Dot,
        seq(
          field("left", $._expression),
          field(
            "operator",
            token(
              seq(
                ".",
                optional(
                  seq(
                    choice(...OperatorChars.filter(x => x != ".")),
                    repeat(choice(...OperatorChars))
                  )
                )
              )
            )
          ),
          field("right", $._expression)
        )
      ),

    unary_expression: $ => {
      /** @param {RuleOrLiteral} op */
      const unaryExp = op => seq(op, field("argument", $._expression));
      return choice(
        prec.left(Precedence.Unary, unaryExp($._unary_operator)),
        prec.left(
          Precedence.Sigil,
          unaryExp(seq("@", repeat(choice(...OperatorChars))))
        )
      );
    },

    _unary_operator: _ =>
      token(
        choice(
          ...Object.values(WordOp),
          seq(
            choice(...OperatorChars.filter(x => x != "@")),
            repeat(choice(...OperatorChars))
          )
        )
      ),

    binary_expression: $ => {
      /** @param {RuleOrLiteral} op */
      const binExp = op =>
        seq(
          field("left", $._expression),
          field("operator", op),
          / +/,
          field("right", $._expression)
        );

      return choice(
        .../** @type {[Rule, number][]} */ ([
          [$._binop10l, Precedence.Op10],
          [$._binop9, Precedence.Op9],
          [WordOp[9], Precedence.Op9],
          [$._binop8, Precedence.Op8],
          [$._binop7, Precedence.Op7],
          [$._binop6, Precedence.Op6],
          [$._binop5, Precedence.Op5],
          [WordOp[5], Precedence.Op5],
          [WordOp[4], Precedence.Op4],
          [WordOp[3], Precedence.Op3],
          [$._binop2, Precedence.Op2],
          [$._binop1, Precedence.Op1],
          [$._binop0, Precedence.Op0],
        ]).map(([operator, precedence]) =>
          prec.left(precedence, binExp(operator))
        ),
        prec.right(Precedence.Op10, binExp($._binop10r))
      );
    },

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

    accent_quoted: $ =>
      seq("`", repeat1(alias(/[^\x00-\x1f\r\n\t` ]+/, $.identifier)), "`"),

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
