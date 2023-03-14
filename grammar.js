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
  ColonEqExpr: 16,
  Sigil: 15,
  Dot: 14,
  Suffix: 13,
  Unary: 12,
  Op10: 11,
  Op9: 10,
  Op8: 9,
  Op7: 8,
  Op6: 7,
  Op5: 6,
  Op4: 5,
  Op3: 4,
  Op2: 3,
  Op1: 2,
  Op0: 1,
};

module.exports = grammar({
  name: "nim",

  word: $ => $.identifier,
  externals: $ => [
    $.comment,
    $._long_string_quote,
    $._layout_start,
    $._layout_end,
    $._invalid_layout,
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
  supertypes: $ => [
    $._simple_expression,
    $._block_expression,
    $._simple_statement,
    $._block_statement,
  ],
  conflicts: $ => [
    [$._command_expression, $.binary_expression],
    [$._command_expression, $.unary_expression, $.binary_expression],
  ],

  rules: {
    source_file: $ => alias($.statement_list, ""),

    statement_list: $ =>
      choice(
        prec.right(
          choice(
            seq(
              sep1($._simple_statement, ";"),
              optional(seq(";", $._block_statement))
            ),
            seq(sep1($._simple_statement, ";"), $._terminator)
          )
        ),
        seq(
          $._layout_start,
          repeat1(
            choice(
              seq($._simple_statement, choice($._terminator, ";")),
              seq($._block_statement)
            )
          ),
          $._layout_end
        )
      ),

    // Any statement that doesn't contain a terminator
    _simple_statement: $ =>
      prec(-1, choice($._simple_expression, alias($._command_call, $.call))),

    // Any statement that contain a block (implicitly terminates)
    _block_statement: $ =>
      choice($._block_expression, alias($._command_call_block, $.call)),

    // All expressions. Use only in rules that doesn't care about termination
    _expression: $ => choice($._simple_expression),

    // Any expression that doesn't contain a terminator
    _simple_expression: $ =>
      choice(
        $._literals,
        $.accent_quoted,
        $.binary_expression,
        $.bracket_expression,
        alias($._simple_call_expression, $.call),
        $.curly_expression,
        $.dot_expression,
        $.parenthesized_expression,
        $.tuple,
        $.unary_expression,
        $.identifier
      ),

    _block_expression: $ => choice(alias($._block_call_expression, $.call)),

    _simple_call_expression: $ =>
      choice($._parenthesized_call, $._command_expression),
    _block_call_expression: $ => choice($._call_with_block, $._block_call),

    _parenthesized_call: $ =>
      prec.right(
        seq(
          field("function", $._simple_expression),
          seq(
            token.immediate("("),
            optional(field("arguments", $.argument_list)),
            ")"
          )
        )
      ),

    _call_with_block: $ =>
      prec(
        1,
        seq(
          $._simple_call_expression,
          field("arguments", $.call_block_arguments)
        )
      ),

    _block_call: $ =>
      prec(
        -10,
        seq(
          field("function", $._simple_expression),
          field("arguments", $.call_block_arguments)
        )
      ),

    _command_expression: $ =>
      prec.right(
        1,
        seq(
          field("function", $._simple_expression),
          field("arguments", alias($._simple_expression, $.argument_list))
        )
      ),

    _command_call: $ =>
      seq(
        field("function", $._simple_expression),
        field("arguments", alias($._command_call_arguments, $.argument_list))
      ),

    _command_call_block: $ =>
      seq($._command_call, field("arguments", $.call_block_arguments)),

    _command_call_arguments: $ =>
      seq(
        $._simple_expression,
        repeat1(prec.right(seq(",", $._maybe_equal_expression)))
      ),

    call_block_arguments: $ =>
      prec.right(
        -10,
        seq(
          ":",
          seqReq1(
            $.statement_list,
            repeat1(
              choice(
                $.of_branch,
                $.elif_branch,
                $.else_branch,
                $.except_branch,
                $.finally_branch
              )
            )
          )
        )
      ),

    argument_list: $ =>
      sep1(choice($._maybe_colon_expression, $._maybe_equal_expression), ","),

    of_branch: $ =>
      seq(
        ignoreStyle("of"),
        field("values", $.expression_list),
        ":",
        $.statement_list
      ),

    elif_branch: $ =>
      seq(
        ignoreStyle("elif"),
        field("condition", $._simple_expression),
        ":",
        $.statement_list
      ),

    else_branch: $ => seq(ignoreStyle("else"), ":", $.statement_list),

    except_branch: $ =>
      seq(
        ignoreStyle("except"),
        optional(field("values", $.expression_list)),
        ":",
        $.statement_list
      ),

    finally_branch: $ => seq(ignoreStyle("finally"), ":", $.statement_list),

    expression_list: $ => sep1($._simple_expression, ","),

    colon_expression: $ =>
      seq(
        field("left", $._simple_expression),
        ":",
        field("right", $._simple_expression)
      ),
    equal_expression: $ =>
      seq(
        field("left", $._simple_expression),
        "=",
        field("right", $._simple_expression)
      ),

    _maybe_colon_expression: $ =>
      choice($._simple_expression, $.colon_expression),
    _maybe_equal_expression: $ =>
      choice($._simple_expression, $.equal_expression),

    tuple: $ =>
      choice(
        seq(
          "(",
          $._simple_expression,
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
          field("left", $._simple_expression),
          "[",
          field("right", $.argument_list),
          "]"
        )
      ),

    curly_expression: $ =>
      prec.left(
        Precedence.Suffix,
        seq(
          field("left", $._simple_expression),
          "{",
          field("right", $.argument_list),
          "}"
        )
      ),

    dot_expression: $ =>
      prec.left(
        Precedence.Dot,
        seq(
          field("left", $._simple_expression),
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
          field("right", $._simple_expression)
        )
      ),

    unary_expression: $ => {
      /** @param {RuleOrLiteral} op */
      const unaryExp = op => seq(op, field("argument", $._simple_expression));
      return choice(
        ...[
          $._wordop9,
          $._wordop5,
          $._wordop4,
          $._wordop3,
          // Avoid precedence clashing with of_branch
          ignoreStyle("of"),
          ignoreStyle("not"),
        ].map(token => prec.left(unaryExp(token))),
        prec.left(
          Precedence.Unary,
          unaryExp(
            token(
              seq(
                choice(...OperatorChars.filter(x => x != "@" && x != ":")),
                repeat(choice(...OperatorChars))
              )
            )
          )
        ),
        prec.left(
          Precedence.Sigil,
          unaryExp(token(seq("@", repeat(choice(...OperatorChars)))))
        )
      );
    },

    binary_expression: $ => {
      /** @param {RuleOrLiteral} op */
      const binExp = op =>
        seq(
          field("left", $._simple_expression),
          field("operator", op),
          field("right", $._simple_expression)
        );

      return choice(
        .../** @type {[Rule, number][]} */ ([
          [$._binop10l, Precedence.Op10],
          [$._binop9, Precedence.Op9],
          [$._wordop9, Precedence.Op9],
          [$._binop8, Precedence.Op8],
          [$._binop7, Precedence.Op7],
          [$._binop6, Precedence.Op6],
          [$._binop5, Precedence.Op5],
          [$._wordop5, Precedence.Op5],
          [$._wordop4, Precedence.Op4],
          [$._wordop3, Precedence.Op3],
          [$._binop2, Precedence.Op2],
          [$._binop1, Precedence.Op1],
          [$._binop0, Precedence.Op0],
        ]).map(([operator, precedence]) =>
          prec.left(precedence, binExp(operator))
        ),
        prec.right(Precedence.Op10, binExp($._binop10r))
      );
    },

    _wordop9: _ =>
      token(choice(...["div", "mod", "shl", "shr"].map(ignoreStyle))),
    _wordop5: _ =>
      token(
        choice(...["in", "notin", "is", "isnot", "as", "from"].map(ignoreStyle))
      ),
    _wordop4: _ => ignoreStyle("and"),
    _wordop3: _ => token(choice(...["or", "xor"].map(ignoreStyle))),

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
        optional(
          token.immediate(
            seq(optional("'"), choice(/[uU]/, /[iIuU](8|16|32|64)/))
          )
        )
      ),

    float_literal: $ => {
      const FloatSuffix = /[fFdD](32|64|128)?/;
      const Apostrophe = optional(token.immediate("'"));

      return choice(
        seq($._numeric_literal, token.immediate(seq(Apostrophe, FloatSuffix))),
        seq(
          $._decimal_float_literal,
          optional(token.immediate(seq(Apostrophe, FloatSuffix)))
        )
      );
    },

    custom_numeric_literal: $ =>
      seq(
        choice($._numeric_literal, $._decimal_float_literal),
        field("function", alias($._custom_numeric_suffix, $.identifier))
      ),

    _custom_numeric_suffix: _ => token.immediate(seq("'", Identifier)),

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
