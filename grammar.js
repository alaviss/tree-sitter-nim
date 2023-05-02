/* Copyright 2023 Leorize <leorize+oss@disroot.org>
 *
 * SPDX-License-Identifier: MPL-2.0
 */

/// <reference types="tree-sitter-cli/dsl" />

const DecimalLiteral = /[0-9](_?[0-9])*/;
const DecimalFloatLiteral = token(
  seq(
    optional("-"),
    choice(
      seq(DecimalLiteral, ".", DecimalLiteral),
      seq(DecimalLiteral, /[eE][+-]?/, DecimalLiteral),
      seq(DecimalLiteral, ".", DecimalLiteral, /[eE][+-]?/, DecimalLiteral)
    )
  )
);
const NumericLiteral = token(
  seq(
    optional("-"),
    choice(
      DecimalLiteral,
      /0[xX][0-9a-fA-F](_?[0-9a-fA-F])*/,
      /0[oO][0-7](_?[0-7])*/,
      /0[bB][01](_?[01])*/
    )
  )
);
const Identifier = /[a-zA-Z\x80-\xff](_?[a-zA-Z0-9\x80-\xff])*/;
const CharEscapeSequence = /[rcnlftv\\"'abe]|\d+|x[0-9a-fA-F]{2}/;
const RawStringLiteral = token.immediate(
  seq('"', optional(/[^\n\r"](""|[^\n\r"])*/), '"')
);
const Templates = {
  /**
   * @template T
   * @param {GrammarSymbols<T>} $
   * @param {RuleOrLiteral} argument_list */
  call: ($, argument_list) =>
    prec(
      "call",
      seq(
        field("function", $._basic_expression),
        field("arguments", alias(argument_list, $.argument_list))
      )
    ),

  /**
   * @template T
   * @param {GrammarSymbols<T>} $
   * @param {RuleOrLiteral} keyword */
  if: ($, keyword) =>
    prec.right(
      seq(
        keyword,
        field("condition", $._expression),
        ":",
        field("consequence", $.statement_list),
        field(
          "alternative",
          repeat(
            choice(
              seq(optional($._line_elif), $.elif_branch),
              seq(optional($._line_else), $.else_branch)
            )
          )
        )
      )
    ),

  /**
   * @template T
   * @param {GrammarSymbols<T>} $
   * @param {RuleOrLiteral} keyword */
  proc_expr: ($, keyword) =>
    seq(keyword, $._routine_type_tail, "=", field("body", $.statement_list)),

  /**
   * @template T
   * @param {GrammarSymbols<T>} $
   * @param {RuleOrLiteral} keyword */
  import: ($, keyword) =>
    seq(
      keyword,
      choice(seq($._expression, $.except_clause), seq($.expression_list))
    ),

  /**
   * @template T
   * @param {GrammarSymbols<T>} $
   * @param {RuleOrLiteral} keyword */
  return_like: ($, keyword) =>
    prec.right(seq(keyword, optional($._expression_with_colon_block_call))),
};
const WordOperators = {
  9: ["div", "mod", "shl", "shr"],
  5: ["in", "notin", "is", "isnot", "not", "of", "as", "from"],
  4: ["and"],
  3: ["or", "xor"],
};

module.exports = grammar({
  name: "nim",

  externals: $ => [
    $.comment,
    $._string_content,
    $._long_string_content,
    $._layout_start,
    $._layout_end,
    $._invalid_layout,
    $._line,
    $._line_elif,
    $._line_else,
    $._line_except,
    $._line_finally,
    $._line_of,
    $._line_do,
    $._binop10l,
    $._binop10r,
    $._binop9,
    $._binop8,
    $._binop7,
    $._binop6,
    $._binop5,
    $._binop2,
    $._sigilop,
    $._binop1,
    $._binop0,
    $._unaryop,
  ],

  extras: $ => [/[\n\r ]+/, $.comment],
  inline: $ => [
    $._maybe_colon_expression,
    $._maybe_equal_expression,
    $._maybe_colon_equal_expression,
    $._symbol,
    $._basic_sigil_expression,
  ],
  precedences: $ => [
    [
      "sigil",
      "suffix",
      "unary",
      "type_modifiers",
      "binary_10",
      "binary_9",
      "binary_8",
      "binary_7",
      "binary_6",
      "binary_5",
      "binary_4",
      "binary_3",
      "binary_2",
      "binary_1",
      "binary_0",
      $._expression_with_colon_block_call,
      // $.equal_expression,
    ],
    ["call"],
    ["sigil", $._basic_expression],
    ["suffix", $._expression_no_unary_word],
    [$._expression_no_unary_word, $._call_colon_block_expression],
    // [$._simple_expression_no_unary_word, $._basic_command],
    // [$._simple_expression_no_unary_word, $.pragma_block],
    // [$._simple_expression, $._expression_no_unary_word],
    // [$._simple_expression, $._expression],
    [$._expression_no_unary_word, $._basic_command],
    [$._expression, $._command_first_argument],
    [$._basic_command, $._command_colon_expression_arguments],
    [$._basic_command, $._command_statement_arguments],
    [$._call_do_block_expression, $._expression_no_unary_word],
    // [$._call_colon_expression, $._simple_expression_no_unary_word],
    // [$._call_do_expression, $._simple_expression_no_unary_word],
    [$._basic_call, $._expression_no_unary_word],
    [$.pragma_block, $._expression_no_unary_word],
    [$.parenthesized, $._expression_with_colon_block_call],
    // [$._command_do_expression_arguments, $._basic_command],
    // [$._call_colon_expression_arguments, $._basic_call],
    // [$._call_do_expression_arguments, $._basic_call],
    [$.proc_expression, $.proc_type],
    [$.iterator_expression, $.iterator_type],
    [$._unary_sigil_argument_list, $._basic_expression],
    [$._unary_argument_list, $.infix_expression],
  ],
  supertypes: $ => [$._statement, $._expression],
  word: $ => $.identifier,

  rules: {
    source_file: $ => $._block_statement_list,

    statement_list: $ =>
      choice($._block_statement_list, $._line_statement_list),

    _line_statement_list: $ => prec.right(sep1($._statement, ";")),
    _block_statement_list: $ =>
      seq(
        $._layout_start,
        repeat(seq($._line, sep1($._statement, ";"))),
        $._layout_end
      ),

    _statement: $ =>
      choice(
        $._expression_with_colon_block_call,
        // $.import_statement,
        // $.export_statement,
        // $.import_from_statement,
        // $.include_statement,
        // $.discard_statement,
        // $.return_statement,
        // $.raise_statement,
        // $.yield_statement,
        // $.break_statement,
        // $.continue_statement,
        // $.assembly_statement,
        // $.bind_statement,
        // $.mixin_statement,
        alias($._command_statement, $.call)
      ),

    /* Command call */
    _command_statement: $ => Templates.call($, $._command_statement_arguments),
    _command_statement_arguments: $ =>
      prec.right(
        seq(
          $._command_first_argument,
          repeat1(prec.right(seq(",", $._maybe_equal_expression))),
          optional(
            choice($._call_colon_block_arguments, $._call_do_block_arguments)
          )
        )
      ),

    /* Statements */
    import_statement: $ => Templates.import($, keyword("import")),
    export_statement: $ => Templates.import($, keyword("export")),
    except_clause: $ => seq(keyword("except"), $.expression_list),
    import_from_statement: $ =>
      seq(
        keyword("from"),
        field("module", $._expression),
        keyword("import"),
        field("symbols", $.expression_list)
      ),
    include_statement: $ => seq(keyword("include"), $.expression_list),
    discard_statement: $ => Templates.return_like($, keyword("discard")),
    return_statement: $ => Templates.return_like($, keyword("return")),
    raise_statement: $ => Templates.return_like($, keyword("raise")),
    yield_statement: $ => Templates.return_like($, keyword("yield")),
    break_statement: $ => Templates.return_like($, keyword("break")),
    continue_statement: $ => Templates.return_like($, keyword("continue")),
    assembly_statement: $ =>
      seq(
        keyword("asm"),
        optional(field("pragma", $._pragma)),
        $.string_literal
      ),
    bind_statement: $ =>
      prec.right(
        seq(keyword("bind"), sep1(choice($._symbol, $.qualified_symbol), ","))
      ),
    mixin_statement: $ =>
      prec.right(
        seq(keyword("mixin"), sep1(choice($._symbol, $.qualified_symbol), ","))
      ),

    /* Expressions */
    _expression_with_colon_block_call: $ =>
      choice($._expression, alias($._call_with_colon_block, $.call)),
    _expression: $ =>
      choice(
        $._expression_no_unary_word,
        alias($._unary_word_expression, $.unary_expression)
      ),
    _expression_no_unary_word: $ =>
      choice(
        $._basic_expression,
        $.infix_expression,
        $.object_type,
        $.distinct_type,
        $.enum_type,
        $.pointer_type,
        $.ref_type,
        $.out_type,
        $.tuple_type,
        $.proc_type,
        $.iterator_type,
        $.if,
        $.when,
        $.case,
        $.try,
        $.block,
        $.pragma_block,
        $.proc_expression,
        $.func_expression,
        $.iterator_expression,
        alias($._call_expression, $.call),
        alias($._unary_symbol_expression, $.unary_expression),
        alias($._pragma, $.pragma)
      ),
    _basic_expression: $ =>
      choice(
        $._basic_sigil_expression,
        alias($._basic_call, $.call),
        alias($._unary_sigil_expression, $.unary_expression)
      ),
    _basic_sigil_expression: $ =>
      choice(
        $._literal,
        $._symbol,
        $.array_construction,
        $.curly_construction,
        $.tuple_construction,
        $.cast,
        $.parenthesized,
        $.dot_expression,
        $.bracket_expression,
        $.curly_expression,
        $.generalized_string
      ),

    infix_expression: $ => {
      /** @param {RuleOrLiteral} op */
      return choice(
        .../** @type {[Rule, string, Function][]} */ ([
          [$._binop10r, "binary_10", prec.right],
          [$._binop10l, "binary_10", prec.left],
          [$._binop9, "binary_9", prec.left],
          [choice(...WordOperators[9].map(keyword)), "binary_9", prec.left],
          [$._binop8, "binary_8", prec.left],
          [$._binop7, "binary_7", prec.left],
          [$._binop6, "binary_6", prec.left],
          [$._binop5, "binary_5", prec.left],
          [choice(...WordOperators[5].map(keyword)), "binary_5", prec.left],
          [
            choice(...WordOperators[4].filter(x => x != "not").map(keyword)),
            "binary_4",
            prec.left,
          ],
          [choice(...WordOperators[3].map(keyword)), "binary_3", prec.left],
          [$._binop2, "binary_2", prec.left],
          [$._binop1, "binary_1", prec.left],
          [$._binop0, "binary_0", prec.left],
        ]).map(([operator, precedence, precFn]) =>
          precFn(
            precedence,
            seq(
              field("left", $._expression),
              field("operator", operator),
              field("right", $._expression),
              field(
                "arguments",
                optional(
                  alias(
                    choice(
                      $._call_colon_block_arguments,
                      $._call_do_block_arguments
                    ),
                    $.argument_list
                  )
                )
              )
            )
          )
        )
      );
    },
    _unary_word_expression: $ =>
      prec.left(
        "unary",
        seq(
          field(
            "operator",
            choice(...Object.values(WordOperators).flatMap(x => x.map(keyword)))
          ),
          field("arguments", $._unary_argument_list)
        )
      ),
    _unary_symbol_expression: $ => {
      /** @param {RuleOrLiteral} op */
      return choice(
        .../** @type {[Rule, string][]} */ ([
          [$._unaryop, "unary"],
          [$._sigilop, "sigil"],
        ]).map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(
              field("operator", operator),
              field("arguments", alias($._unary_argument_list, $.argument_list))
            )
          )
        )
      );
    },
    _unary_argument_list: $ =>
      prec.right(
        seq(
          $._expression,
          optional(
            choice($._call_colon_block_arguments, $._call_do_block_arguments)
          )
        )
      ),
    _unary_sigil_expression: $ =>
      prec.left(
        "sigil",
        seq(
          field("operator", $._sigilop),
          field(
            "argument",
            alias($._unary_sigil_argument_list, $.argument_list)
          )
        )
      ),
    _unary_sigil_argument_list: $ =>
      prec.right(
        seq(
          $._basic_sigil_expression,
          optional(
            choice($._call_colon_block_arguments, $._call_do_block_arguments)
          )
        )
      ),

    /* Call expressions */
    _call_with_colon_block: $ =>
      choice(
        $._call_colon_expression,
        $._call_colon_block_expression,
        $._command_colon_expression
      ),
    _call_colon_expression: $ =>
      Templates.call($, $._call_colon_expression_arguments),
    _call_colon_expression_arguments: $ =>
      seq($._parenthesized_argument_list, $._call_colon_block_arguments),
    _call_colon_block_expression: $ =>
      Templates.call($, $._call_colon_block_arguments),
    _command_colon_expression: $ =>
      Templates.call($, $._command_colon_expression_arguments),
    _command_colon_expression_arguments: $ =>
      seq($._command_first_argument, $._call_colon_block_arguments),
    _call_colon_block_arguments: $ =>
      prec.right(
        seq(
          ":",
          optional($.statement_list),
          repeat(
            choice(
              seq($._line_of, $.of_branch),
              seq($._line_elif, $.elif_branch),
              seq($._line_else, $.else_branch),
              seq($._line_except, $.except_branch),
              seq($._line_finally, $.finally_branch),
              seq($._line_do, $.do_block)
            )
          )
        )
      ),

    _call_expression: $ =>
      choice(
        $._basic_command,
        $._call_do_expression,
        $._call_do_block_expression,
        $._command_do_expression
      ),
    _call_do_expression: $ =>
      Templates.call($, $._call_do_expression_arguments),
    _call_do_expression_arguments: $ =>
      seq($._parenthesized_argument_list, $._call_do_block_arguments),
    _call_do_block_expression: $ =>
      Templates.call($, $._call_do_block_arguments),
    _command_do_expression: $ =>
      Templates.call($, $._command_do_expression_arguments),
    _command_do_expression_arguments: $ =>
      seq($._command_first_argument, $._call_do_block_arguments),
    _call_do_block_arguments: $ =>
      prec.right(
        seq(
          $.do_block,
          repeat(
            choice(
              seq($._line_of, $.of_branch),
              seq($._line_elif, $.elif_branch),
              seq($._line_else, $.else_branch),
              seq($._line_except, $.except_branch),
              seq($._line_finally, $.finally_branch),
              seq($._line_do, $.do_block)
            )
          )
        )
      ),
    _basic_call: $ =>
      prec.right("call", Templates.call($, $._parenthesized_argument_list)),
    _basic_command: $ =>
      prec.right("call", Templates.call($, $._command_first_argument)),
    _command_first_argument: $ => $._expression_no_unary_word,

    /* Conditionals */
    if: $ => Templates.if($, keyword("if")),
    when: $ => Templates.if($, keyword("when")),
    case: $ =>
      prec.right(
        seq(
          keyword("case"),
          $._expression,
          optional(":"),
          repeat1(
            choice(
              seq($._line_of, $.of_branch),
              seq($._line_elif, $.elif_branch),
              seq($._line_else, $.else_branch)
            )
          )
        )
      ),
    try: $ =>
      prec.right(
        seq(
          keyword("try"),
          ":",
          field("body", $.statement_list),
          repeat1(
            choice(
              seq($._line_except, $.except_branch),
              seq($._line_finally, $.finally_branch)
            )
          )
        )
      ),
    of_branch: $ =>
      seq(
        keyword("of"),
        field("values", $.expression_list),
        ":",
        $.statement_list
      ),
    elif_branch: $ =>
      seq(
        keyword("elif"),
        field("condition", $._expression),
        ":",
        field("consequence", $.statement_list)
      ),
    else_branch: $ => seq(keyword("else"), ":", $.statement_list),
    except_branch: $ =>
      seq(
        keyword("except"),
        optional(field("values", $.expression_list)),
        ":",
        $.statement_list
      ),
    finally_branch: $ => seq(keyword("finally"), ":", $.statement_list),
    do_block: $ =>
      seq(
        keyword("do"),
        optional(seq("(", $.parameter_declaration_list, ")")),
        optional(seq("->", $._expression)),
        ":",
        field("body", $.statement_list)
      ),

    /* Structural */
    block: $ =>
      seq(
        keyword("block"),
        optional(field("label", $._symbol)),
        ":",
        field("body", $.statement_list)
      ),
    pragma_block: $ => seq($._pragma, ":", field("body", $.statement_list)),

    cast: $ =>
      seq(
        keyword("cast"),
        field("type", optional(seq("[", $._expression, "]"))),
        field("value", seq("(", $._maybe_colon_equal_expression, ")"))
      ),

    parenthesized: $ =>
      choice(
        seq("(", optional(";"), $.statement_list, ")"),
        seq("(", $._expression, ")")
      ),

    /* Suffix expressions */
    dot_expression: $ =>
      prec(
        "suffix",
        seq(field("left", $._basic_expression), ".", field("right", $._symbol))
      ),
    bracket_expression: $ =>
      prec(
        "suffix",
        seq(
          field("left", $._basic_expression),
          token.immediate("["),
          field("right", alias($._argument_list, $.argument_list)),
          "]"
        )
      ),
    curly_expression: $ =>
      prec(
        "suffix",
        seq(
          field("left", $._basic_expression),
          token.immediate("{"),
          field("right", alias($._argument_list, $.argument_list)),
          "}"
        )
      ),
    qualified_symbol: $ =>
      seq(field("left", $._symbol), ".", field("right", $._symbol)),

    /* Routine expressions */
    proc_expression: $ => Templates.proc_expr($, keyword("proc")),
    func_expression: $ => Templates.proc_expr($, keyword("func")),
    iterator_expression: $ => Templates.proc_expr($, keyword("iterator")),

    /* Type expressions */
    object_type: () => keyword("object"),
    enum_type: () => keyword("enum"),
    distinct_type: $ =>
      prec("type_modifiers", seq(keyword("distinct"), $._expression)),
    pointer_type: $ =>
      prec("type_modifiers", seq(keyword("ptr"), $._expression)),
    ref_type: $ => prec("type_modifiers", seq(keyword("ref"), $._expression)),
    var_type: $ => prec("type_modifiers", seq(keyword("var"), $._expression)),
    out_type: $ => prec("type_modifiers", seq(keyword("out"), $._expression)),
    tuple_type: $ =>
      prec.right(
        seq(keyword("tuple"), optional(seq("[", $.field_declaration_list, "]")))
      ),
    proc_type: $ => seq(keyword("proc"), $._routine_type_tail),
    iterator_type: $ => seq(keyword("iterator"), $._routine_type_tail),
    _routine_type_tail: $ =>
      prec.right(
        seq(
          "(",
          $.parameter_declaration_list,
          ")",
          optional(seq(":", $._expression)),
          optional($._pragma)
        )
      ),

    /* Literal construction */
    array_construction: $ =>
      seq("[", optional(sep1($._maybe_colon_equal_expression, ",")), "]"),
    curly_construction: $ =>
      choice(
        seq("{", sep1($._maybe_colon_equal_expression, ","), "}"),
        seq("{", optional(":"), "}")
      ),
    tuple_construction: $ =>
      choice(
        seq("(", optional($.colon_expression), ")"),
        seq(
          "(",
          $._maybe_colon_expression,
          repeat1(seq(",", optional($._maybe_colon_expression))),
          ")"
        )
      ),
    generalized_string: $ =>
      seq(
        choice($.identifier, $.dot_expression),
        alias($._generalized_string_literal, $.string_literal)
      ),
    _generalized_string_literal: $ =>
      choice(
        seq(token.immediate('"""'), $._long_string_body),
        RawStringLiteral
      ),
    _pragma: $ => seq("{.", alias($._argument_list, $.pragma_list), /\.?\}/),

    /* Supporting expressions */
    _parenthesized_argument_list: $ =>
      seq(token.immediate("("), optional($._argument_list), ")"),
    _maybe_colon_expression: $ => choice($._expression, $.colon_expression),
    _maybe_equal_expression: $ => choice($._expression, $.equal_expression),
    _maybe_colon_equal_expression: $ =>
      choice($._expression, $.colon_expression, $.equal_expression),
    _argument_list: $ => sep1($._maybe_colon_equal_expression, ","),
    colon_expression: $ =>
      seq(field("left", $._expression), ":", field("right", $._expression)),
    equal_expression: $ =>
      prec.right(
        seq(field("left", $._expression), "=", field("right", $._expression))
      ),
    expression_list: $ =>
      prec.right(
        seq($._expression, repeat(prec.right(seq(",", $._expression))))
      ),

    /* Symbol/identifier declaration */
    parameter_declaration_list: $ =>
      sep1(
        alias($._identifier_declaration, $.parameter_declaration),
        choice(",", ";")
      ),
    field_declaration_list: $ =>
      sep1(
        alias($._identifier_declaration, $.field_declaration),
        choice(",", ";")
      ),
    _identifier_declaration: $ =>
      seq(
        $.symbol_declaration_list,
        field("type", optional(seq(":", $._expression))),
        field("value", optional(seq("=", $._expression_with_colon_block_call)))
      ),
    symbol_declaration_list: $ =>
      prec.right(
        sep1(choice($.symbol_declaration, $.tuple_deconstruct_declaration), ",")
      ),
    tuple_deconstruct_declaration: $ =>
      seq("(", sep1($.symbol_declaration, ","), ")"),
    symbol_declaration: $ =>
      seq(
        field("name", $._symbol),
        optional($.export_marker),
        optional($._pragma)
      ),
    export_marker: () => "*",

    /* Literals */
    _literal: $ =>
      choice(
        $.boolean_literal,
        $.nil_literal,
        $.integer_literal,
        $.float_literal,
        $.custom_numeric_literal,
        $.char_literal,
        $.string_literal
      ),

    /* "boolean" literals doesn't exist in the grammar, but these are
     * useful to keep around. */
    boolean_literal: () =>
      choice(keyword("true"), keyword("false"), keyword("on"), keyword("off")),
    nil_literal: () => keyword("nil"),

    integer_literal: () =>
      token(seq(NumericLiteral, optional(/'?[iIuU](8|16|32|64)|[uU]/))),

    float_literal: () => {
      const FloatSuffix = /'?[fFdD](32|64|128)?/;

      return token(
        choice(
          seq(NumericLiteral, FloatSuffix),
          seq(DecimalFloatLiteral, optional(FloatSuffix))
        )
      );
    },

    custom_numeric_literal: () =>
      token(seq(choice(NumericLiteral, DecimalFloatLiteral), "'", Identifier)),

    char_literal: () =>
      token(seq("'", choice(/[^\\']/, seq("\\", CharEscapeSequence)), "'")),

    string_literal: $ =>
      choice(
        $._interpreted_string_literal,
        $._raw_string_literal,
        $._long_string_literal
      ),

    _interpreted_string_literal: $ =>
      seq(
        '"',
        repeat(choice($._string_content, $.escape_sequence)),
        token.immediate('"')
      ),

    escape_sequence: () =>
      token(
        seq(
          "\\",
          choice(CharEscapeSequence, "p", /u([0-9a-fA-F]{4}|\{[0-9a-fA-F]+\})/)
        )
      ),

    _raw_string_literal: () => token(seq(/[rR]/, RawStringLiteral)),

    _long_string_literal: $ => seq(/[rR]?"""/, $._long_string_body),

    _long_string_body: $ =>
      seq(repeat($._long_string_content), token.immediate('"""')),

    /* Identifiers and related shenanigans */
    _symbol: $ => choice($.accent_quoted, $.identifier),
    accent_quoted: $ =>
      /* eslint-disable-next-line no-control-regex */
      seq("`", repeat1(alias(/[^\x00-\x1f\r\n\t` ]+/, $.identifier)), "`"),
    identifier: () => token(choice(Identifier, "_")),
  },
});

/**
 * Produce a rule to match nim-style style insensitive identifiers
 *
 * @param {string} ident - The string to match style-insensitively.
 */
function keyword(ident) {
  const regex = ident
    .split("")
    .map((letter, idx) =>
      idx > 0 ? `[${letter.toLowerCase()}${letter.toUpperCase()}]` : letter
    )
    .join("_?");

  return alias(token(prec(1, new RegExp(regex))), ident);
}

/**
 * Produce a rule that matches one or more occurance of a given rule.
 * Each of the rules are separated by the given separator.
 * @param {RuleOrLiteral} rule - The rule to be matched.
 * @param {RuleOrLiteral} sep - The matched separator.
 */
function sep1(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}
