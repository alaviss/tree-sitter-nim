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
    $._line,
    $._line_elif,
    $._line_else,
    $._line_except,
    $._line_finally,
    $._line_of,
    $._line_do,
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
    $._expression,
    $._simple_statement,
    $._statement,
  ],
  conflicts: $ => [
    [$._command_expression, $.binary_expression],
    [$._command_expression, $.unary_expression, $.binary_expression],
  ],
  precedences: $ => [
    [$._symbol, $._simple_expression], // break conflict between var type and var section
    [$.object_declaration, $.object_type],
  ],

  rules: {
    source_file: $ =>
      choice(alias($.statement_list, ""), seq($._layout_start, $._layout_end)),

    statement_list: $ =>
      choice(
        prec.right(sep1($._simple_statement, ";")),
        $._block_statement_list
      ),

    _block_statement_list: $ =>
      seq(
        $._layout_start,
        repeat1(seq($._line, sep1($._statement, ";"))),
        $._layout_end
      ),

    _statement: $ =>
      choice(
        $._expression,
        $._declaration,
        alias($._command_call_block, $.call),
        $._simple_statement,
        $.bind_statement,
        $.mixin_statement,
        $.while,
        $.for,
        $.static_statement
      ),

    bind_statement: $ =>
      seq(
        ignoreStyle("bind"),
        sep1(choice($._symbol, $.qualified_symbol), ",")
      ),

    mixin_statement: $ =>
      seq(
        ignoreStyle("mixin"),
        sep1(choice($._symbol, $.qualified_symbol), ",")
      ),

    qualified_symbol: $ =>
      prec.left(
        Precedence.Dot,
        seq(
          field("left", $._symbol),
          field("operator", "."),
          field("right", $._symbol)
        )
      ),

    while: $ =>
      seq(
        ignoreStyle("while"),
        field("condition", $._simple_expression),
        ":",
        field("body", $.statement_list)
      ),

    for: $ =>
      seq(
        ignoreStyle("for"),
        field("left", $.symbol_declaration_list),
        ignoreStyle("in"),
        field("right", $._simple_expression),
        ":",
        field("body", $.statement_list)
      ),

    static_statement: $ =>
      seq(ignoreStyle("static"), ":", field("body", $.statement_list)),

    _simple_statement: $ =>
      prec(
        -1,
        choice(
          $._simple_expression,
          alias($._command_call, $.call),
          alias($._pragma, $.pragma),
          $.import_statement,
          $.export_statement,
          $.import_from_statement,
          $.include_statement,
          $.discard_statement,
          $.assembly_statement,
          $.break_statement,
          $.continue_statement,
          $.return_statement,
          $.raise_statement,
          $.yield_statement
        )
      ),

    import_statement: $ =>
      prec.right(
        seq(
          ignoreStyle("import"),
          $._simple_expression,
          optional(
            choice(
              repeat1(seq(",", $._simple_expression)),
              seq(
                ignoreStyle("except"),
                alias($.expression_list, $.import_exception_list)
              )
            )
          )
        )
      ),

    export_statement: $ =>
      prec.right(
        seq(
          ignoreStyle("export"),
          $._simple_expression,
          optional(
            choice(
              repeat1(seq(",", $._simple_expression)),
              seq(
                ignoreStyle("except"),
                alias($.expression_list, $.export_exception_list)
              )
            )
          )
        )
      ),

    import_from_statement: $ =>
      seq(
        ignoreStyle("from"),
        field("module", $._simple_expression),
        ignoreStyle("import"),
        alias($.expression_list, $.import_symbol_list)
      ),

    include_statement: $ =>
      seq(ignoreStyle("include"), alias($.expression_list, "include files")),

    discard_statement: $ =>
      prec.right(seq(ignoreStyle("discard"), optional($._expression))),

    assembly_statement: $ =>
      seq(
        ignoreStyle("asm"),
        optional(field("pragma", $._pragma)),
        $.string_literal
      ),

    break_statement: $ =>
      prec.right(
        seq(ignoreStyle("break"), optional(field("label", $._expression)))
      ),

    continue_statement: $ =>
      prec.right(
        seq(ignoreStyle("continue"), optional(field("label", $._expression)))
      ),

    return_statement: $ =>
      prec.right(seq(ignoreStyle("return"), optional($._expression))),

    raise_statement: $ =>
      prec.right(seq(ignoreStyle("raise"), optional($._expression))),

    yield_statement: $ =>
      prec.right(seq(ignoreStyle("yield"), optional($._expression))),

    _declaration: $ =>
      choice(
        $.const_section,
        $.let_section,
        $.type_section,
        $.var_section,
        $.proc_declaration,
        $.func_declaration,
        $.method_declaration,
        $.iterator_declaration,
        $.macro_declaration,
        $.template_declaration,
        $.converter_declaration
      ),

    proc_declaration: $ => seq(ignoreStyle("proc"), $._routine_declaration),
    func_declaration: $ => seq(ignoreStyle("func"), $._routine_declaration),
    method_declaration: $ => seq(ignoreStyle("method"), $._routine_declaration),
    iterator_declaration: $ =>
      seq(ignoreStyle("iterator"), $._routine_declaration),
    macro_declaration: $ => seq(ignoreStyle("macro"), $._routine_declaration),
    template_declaration: $ =>
      seq(ignoreStyle("template"), $._routine_declaration),
    converter_declaration: $ =>
      seq(ignoreStyle("converter"), $._routine_declaration),

    _routine_declaration: $ =>
      seq(
        field("name", $._maybe_exported_symbol),
        optional(field("rewrite_pattern", $.term_rewriting_pattern)),
        optional($._generic_parameter_list),
        optional(
          seq("(", optional(field("parameters", $.parameter_list)), ")")
        ),
        optional(seq(":", field("return_type", $._simple_expression))),
        optional(field("pragma", $._pragma)),
        optional(seq("=", field("body", $.statement_list)))
      ),

    term_rewriting_pattern: $ => seq("{", $._statement, "}"),

    type_section: $ =>
      seq(
        ignoreStyle("type"),
        choice(
          $.type_declaration,
          seq(
            $._layout_start,
            repeat1(seq($._line, $.type_declaration)),
            $._layout_end
          )
        )
      ),

    type_declaration: $ =>
      seq($.type_symbol_declaration, "=", $._type_definition),

    type_symbol_declaration: $ =>
      prec.right(
        seq(
          $._maybe_exported_symbol,
          optional($._generic_parameter_list),
          optional(field("pragma", $._pragma))
        )
      ),

    _generic_parameter_list: $ =>
      seq("[", alias($.parameter_list, $.generic_parameter_list), "]"),

    parameter_list: $ =>
      sep1(
        alias($.variable_declaration, $.parameter_declaration),
        token(choice(",", ";"))
      ),

    _type_definition: $ =>
      choice(
        $._simple_expression,
        $._object_like_declaration,
        alias($._distinct_declaration, $.distinct_type),
        alias($._pointer_declaration, $.pointer_type),
        alias($._ref_declaration, $.ref_type),
        $.concept_declaration
      ),

    concept_declaration: $ =>
      seq(
        ignoreStyle("concept"),
        optional(
          field(
            "parameters",
            alias($._concept_parameter_list, $.parameter_list)
          )
        ),
        optional(seq(ignoreStyle("of"), field("refines", $.refinement_list))),
        field("body", alias($._block_statement_list, $.statement_list))
      ),

    _concept_parameter_list: $ => sep1($._concept_parameter, ","),

    _concept_parameter: $ =>
      choice(
        $._symbol,
        alias($._concept_pointer_parameter, $.pointer_parameter),
        alias($._concept_ref_parameter, $.ref_parameter),
        alias($._concept_static_parameter, $.static_parameter),
        alias($._concept_type_parameter, $.type_parameter),
        alias($._concept_var_parameter, $.var_parameter)
      ),

    _concept_pointer_parameter: $ => seq(ignoreStyle("ptr"), $._symbol),
    _concept_ref_parameter: $ => seq(ignoreStyle("ref"), $._symbol),
    _concept_static_parameter: $ => seq(ignoreStyle("static"), $._symbol),
    _concept_type_parameter: $ => seq(ignoreStyle("type"), $._symbol),
    _concept_var_parameter: $ => seq(ignoreStyle("var"), $._symbol),

    refinement_list: $ => sep1($._simple_expression, ","),

    _object_like_declaration: $ =>
      choice($.object_declaration, alias($._tuple_declaration, $.tuple_type)),

    _distinct_declaration: $ =>
      seq(ignoreStyle("distinct"), choice($._object_like_declaration)),

    _pointer_declaration: $ =>
      seq(ignoreStyle("ptr"), choice($._object_like_declaration)),

    _ref_declaration: $ =>
      seq(ignoreStyle("ref"), choice($._object_like_declaration)),

    object_declaration: $ =>
      seq(
        ignoreStyle("object"),
        optional(field("pragma", $._pragma)),
        optional(
          seq(ignoreStyle("of"), field("inherits", $._simple_expression))
        ),
        optional($.field_declaration_list)
      ),

    _tuple_declaration: $ =>
      seq(
        ignoreStyle("tuple"),
        $._layout_start,
        repeat1(
          seq($._line, alias($.variable_declaration, $.field_declaration))
        ),
        $._layout_end
      ),

    field_declaration_list: $ =>
      seq(
        $._layout_start,
        repeat1(seq($._line, $._object_field_declaration)),
        $._layout_end
      ),

    _object_field_declaration: $ =>
      choice(
        alias($.variable_declaration, $.field_declaration),
        $.conditional_declaration,
        $.variant_declaration,
        $.nil_literal,
        $.discard_statement
      ),

    conditional_declaration: $ =>
      prec.right(
        seq(
          ignoreStyle("when"),
          field("condition", $._simple_expression),
          ":",
          field("consequence", $.field_declaration_list),
          repeat(
            field(
              "alternative",
              seq(
                optional($._line_elif),
                alias($._object_elif_branch, $.elif_branch)
              )
            )
          ),
          optional(
            field(
              "alternative",
              seq(
                optional($._line_else),
                alias($._variant_else_branch, $.else_branch)
              )
            )
          )
        )
      ),

    _object_elif_branch: $ =>
      seq(
        ignoreStyle("elif"),
        field("condition", $._simple_expression),
        ":",
        field("consequence", $.field_declaration_list)
      ),

    variant_declaration: $ =>
      seq(
        ignoreStyle("case"),
        $.variant_descriminator_declaration,
        optional(":"),
        choice(
          $._variant_body,
          seq($._layout_start, $._variant_body, $._layout_end)
        )
      ),

    _variant_body: $ =>
      seqReq1(
        repeat1(seq($._line_of, alias($._variant_of_branch, $.of_branch))),
        seq($._line_else, alias($._variant_else_branch, $.else_branch))
      ),

    _variant_of_branch: $ =>
      seq(
        ignoreStyle("of"),
        field("values", $.expression_list),
        ":",
        $.field_declaration_list
      ),

    _variant_else_branch: $ =>
      seq(ignoreStyle("else"), ":", $.field_declaration_list),

    variant_descriminator_declaration: $ =>
      prec.left(
        seq(
          $.symbol_declaration,
          ":",
          field("type", $._simple_expression),
          optional(":")
        )
      ),

    const_section: $ =>
      seq(ignoreStyle("const"), $._variable_declaration_section),

    let_section: $ => seq(ignoreStyle("let"), $._variable_declaration_section),

    var_section: $ => seq(ignoreStyle("var"), $._variable_declaration_section),

    _variable_declaration_section: $ =>
      choice(
        $.variable_declaration,
        seq(
          $._layout_start,
          repeat1(seq($._line, $.variable_declaration)),
          $._layout_end
        )
      ),

    variable_declaration: $ =>
      prec.right(
        seq(
          $.symbol_declaration_list,
          optional(seq(":", field("type", $._simple_expression))),
          optional(seq("=", field("value", $._expression)))
        )
      ),

    symbol_declaration_list: $ =>
      sep1(choice($.symbol_declaration, $.tuple_deconstruct_declaration), ","),

    tuple_deconstruct_declaration: $ =>
      seq("(", sep1($.symbol_declaration, ","), ")"),

    symbol_declaration: $ =>
      prec.right(
        seq($._maybe_exported_symbol, optional(field("pragma", $._pragma)))
      ),

    _maybe_exported_symbol: $ =>
      choice(field("name", $._symbol), $.exported_symbol),

    exported_symbol: $ => seq(field("name", $._symbol), "*"),

    _symbol: $ =>
      seq(choice($.identifier, $.accent_quoted, $.blank_identifier)),

    _expression: $ =>
      choice(
        alias($._block_call_expression, $.call),
        $._simple_expression,
        $.block,
        $.case,
        $.if,
        $.pragma_block,
        $.try,
        $.when,
        $.proc_expression,
        $.func_expression,
        $.iterator_expression
      ),

    proc_expression: $ => seq(ignoreStyle("proc"), $._routine_expression),
    func_expression: $ => seq(ignoreStyle("func"), $._routine_expression),
    iterator_expression: $ =>
      seq(ignoreStyle("iterator"), $._routine_expression),

    _routine_expression: $ =>
      seq(
        "(",
        optional(field("parameters", $.parameter_list)),
        ")",
        optional(seq(":", field("return_type", $._simple_expression))),
        optional(field("pragma", $._pragma)),
        optional(seq("=", field("body", $.statement_list)))
      ),

    try: $ =>
      prec.right(
        seq(
          ignoreStyle("try"),
          ":",
          field("body", $.statement_list),
          seqReq1(
            repeat1(seq(optional($._line_except), $.except_branch)),
            seq(optional($._line_finally), $.finally_branch)
          )
        )
      ),

    case: $ =>
      seq(
        ignoreStyle("case"),
        field("value", $._simple_expression),
        optional(":"),
        choice($._case_body, seq($._layout_start, $._case_body, $._layout_end))
      ),

    _case_body: $ =>
      prec.right(
        seqReq1(
          repeat1(seq($._line_of, $.of_branch)),
          repeat1(seq($._line_elif, $.elif_branch)),
          seq($._line_else, $.else_branch)
        )
      ),

    when: $ =>
      prec.right(
        seq(
          ignoreStyle("when"),
          field("condition", $._simple_expression),
          ":",
          field("consequence", $.statement_list),
          repeat(
            field("alternative", seq(optional($._line_elif), $.elif_branch))
          ),
          optional(
            field("alternative", seq(optional($._line_else), $.else_branch))
          )
        )
      ),

    if: $ =>
      prec.right(
        seq(
          ignoreStyle("if"),
          field("condition", $._simple_expression),
          ":",
          field("consequence", $.statement_list),
          repeat(
            field("alternative", seq(optional($._line_elif), $.elif_branch))
          ),
          optional(
            field("alternative", seq(optional($._line_else), $.else_branch))
          )
        )
      ),

    block: $ =>
      seq(
        ignoreStyle("block"),
        optional(field("label", $._simple_expression)),
        ":",
        field("body", $.statement_list)
      ),

    _simple_expression: $ =>
      choice(
        $._literals,
        $.accent_quoted,
        $.binary_expression,
        $.bracket,
        $.bracket_expression,
        alias($._simple_call_expression, $.call),
        $.curly,
        $.curly_expression,
        $.dot_expression,
        $.parenthesized_expression,
        $.tuple,
        $.tuple_type,
        $.object_type,
        $.distinct_type,
        $.pointer_type,
        $.ref_type,
        $.var_type,
        $.unary_expression,
        $.identifier
      ),

    tuple_type: $ =>
      prec.right(
        seq(
          ignoreStyle("tuple"),
          optional(
            seq(
              token.immediate("["),
              sep1(
                alias($.variable_declaration, $.field_declaration),
                token(choice(",", ";"))
              ),
              "]"
            )
          )
        )
      ),

    object_type: _ => ignoreStyle("object"),

    distinct_type: $ =>
      prec.right(seq(ignoreStyle("distinct"), $._simple_expression)),
    pointer_type: $ =>
      prec.right(seq(ignoreStyle("ptr"), $._simple_expression)),
    ref_type: $ => prec.right(seq(ignoreStyle("ref"), $._simple_expression)),
    var_type: $ => prec.right(seq(ignoreStyle("var"), $._simple_expression)),

    pragma_block: $ => seq($._pragma, ":", field("body", $.statement_list)),

    _pragma: $ =>
      seq(
        "{.",
        alias($.argument_list, $.pragma_list),
        token(choice(".}", "}"))
      ),

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
        seqReq1(
          seq(
            ":",
            seqReq1(
              $.statement_list,
              repeat1(
                seq(
                  choice(
                    seq($._line_of, $.of_branch),
                    seq($._line_elif, $.elif_branch),
                    seq($._line_else, $.else_branch),
                    seq($._line_except, $.except_branch),
                    seq($._line_finally, $.finally_branch)
                  )
                )
              )
            )
          ),
          repeat1(seq(optional($._line_do), $.do_block))
        )
      ),

    do_block: $ =>
      seq(
        ignoreStyle("do"),
        optional(
          seq("(", optional(field("parameters", $.parameter_list)), ")")
        ),
        optional(seq("->", field("return_type", $._simple_expression))),
        optional(field("pragma", $._pragma)),
        ":",
        field("body", $.statement_list)
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
        field("consequence", $.statement_list)
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

    bracket: $ => seq("[", alias($.argument_list, "bracket list"), "]"),

    curly: $ => seq("{", alias($.argument_list, "curly list"), "}"),

    parenthesized_expression: $ => seq("(", $._expression, ")"),

    bracket_expression: $ =>
      prec.left(
        Precedence.Suffix,
        seq(
          field("left", $._simple_expression),
          token.immediate("["),
          field("right", $.argument_list),
          "]"
        )
      ),

    curly_expression: $ =>
      prec.left(
        Precedence.Suffix,
        seq(
          field("left", $._simple_expression),
          token.immediate("{"),
          field("right", $.argument_list),
          "}"
        )
      ),

    dot_expression: $ =>
      prec.left(
        Precedence.Dot,
        seq(
          field("left", $._simple_expression),
          field("operator", "."),
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
          // Avoid precedence clashing with import_from
          ignoreStyle("from"),
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
          [ignoreStyle("from"), Precedence.Op5],
          [ignoreStyle("of"), Precedence.Op5],
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
      token(choice(...["in", "notin", "is", "isnot", "as"].map(ignoreStyle))),
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

    blank_identifier: _ => "_",

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
