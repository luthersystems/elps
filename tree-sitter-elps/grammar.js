/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// Character classes matching lexer.go
// miscWordSymbols = "._+-*/=<>!&~%?$"
// miscWordRunes   = miscWordSymbols + "0123456789"
// isWordStart: unicode.IsLetter || miscWordSymbols
// isWord: unicode.IsLetter || miscWordRunes

const SYMBOL_START = /[a-zA-Z._+\-*/=<>!&~%?$\u00C0-\u024F\u0370-\u03FF\u0400-\u04FF]/;
const SYMBOL_CONT = /[a-zA-Z0-9._+\-*/=<>!&~%?$\u00C0-\u024F\u0370-\u03FF\u0400-\u04FF]/;

module.exports = grammar({
  name: "elps",

  extras: $ => [/\s+/, $.comment],

  externals: $ => [$.raw_string, $.string],

  conflicts: $ => [],

  rules: {
    source_file: $ =>
      seq(optional($.hashbang), repeat($._expression)),

    hashbang: _ => seq("#!", /[^\n]*/),

    _expression: $ =>
      choice(
        // Atoms
        $.integer,
        $.float,
        $.octal_integer,
        $.hex_integer,
        $.string,
        $.raw_string,
        $.symbol,
        $.qualified_symbol,
        $.keyword,

        // Compound forms
        $.nil,
        $.list,
        $.bracket_list,

        // Prefix forms
        $.quote,
        $.function_quote,
        $.expr_shorthand,

        // Semantic forms (higher precedence)
        $.defun_form,
        $.defmacro_form,
        $.deftype_form,
        $.lambda_form,
        $.let_form,
        $.defconst_form,
      ),

    // --- Atoms ---

    integer: _ => token(seq(optional("-"), /[0-9]+/)),

    float: _ =>
      token(
        seq(
          optional("-"),
          /[0-9]+/,
          choice(
            seq(".", /[0-9]+/, optional(seq(/[eE]/, optional(/[+-]/), /[0-9]+/))),
            seq(/[eE]/, optional(/[+-]/), /[0-9]+/),
          ),
        ),
      ),

    octal_integer: _ =>
      token(seq(optional("-"), /\#[oO]/, /[0-7]+/)),

    hex_integer: _ =>
      token(seq(optional("-"), /\#[xX]/, /[0-9a-fA-F]+/)),

    // string and raw_string are both handled by the external scanner
    // (src/scanner.c) to disambiguate "" (empty string) vs """...""" (raw string).
    // The internal tokenizer would greedily match "" as an empty string,
    // preventing the external scanner from recognizing a raw string opening.

    symbol: _ =>
      token(seq(SYMBOL_START, repeat(SYMBOL_CONT))),

    qualified_symbol: _ =>
      token(
        seq(
          SYMBOL_START,
          repeat(SYMBOL_CONT),
          ":",
          SYMBOL_START,
          repeat(SYMBOL_CONT),
        ),
      ),

    keyword: _ =>
      token(seq(":", SYMBOL_START, repeat(SYMBOL_CONT))),

    // --- Compound forms ---

    nil: _ => seq("(", ")"),

    list: $ =>
      seq("(", repeat1($._expression), ")"),

    bracket_list: $ =>
      seq("[", repeat($._expression), "]"),

    // --- Prefix forms ---

    quote: $ => seq("'", $._expression),

    function_quote: $ =>
      seq(
        "#'",
        choice($.symbol, $.qualified_symbol),
      ),

    expr_shorthand: $ => seq("#^", $._expression),

    // --- Semantic forms ---
    // These match specific definition patterns. They use prec(1, ...) to
    // take priority over the generic `list` rule when the head symbol matches.

    defun_form: $ =>
      prec(
        1,
        seq(
          "(",
          field("operator", alias("defun", $.symbol)),
          field("name", choice($.symbol, $.qualified_symbol, $.quote)),
          field("formals", $.formals),
          repeat(field("body", $._expression)),
          ")",
        ),
      ),

    defmacro_form: $ =>
      prec(
        1,
        seq(
          "(",
          field("operator", alias("defmacro", $.symbol)),
          field("name", choice($.symbol, $.qualified_symbol, $.quote)),
          field("formals", $.formals),
          repeat(field("body", $._expression)),
          ")",
        ),
      ),

    deftype_form: $ =>
      prec(
        1,
        seq(
          "(",
          field("operator", alias("deftype", $.symbol)),
          field("name", choice($.symbol, $.qualified_symbol, $.quote)),
          field("formals", $.formals),
          repeat(field("body", $._expression)),
          ")",
        ),
      ),

    lambda_form: $ =>
      prec(
        1,
        seq(
          "(",
          field("operator", alias("lambda", $.symbol)),
          field("formals", $.formals),
          repeat(field("body", $._expression)),
          ")",
        ),
      ),

    let_form: $ =>
      prec(
        1,
        seq(
          "(",
          field("operator", alias(choice("let", "let*", "flet", "labels", "macrolet"), $.symbol)),
          field("bindings", $.let_bindings),
          repeat(field("body", $._expression)),
          ")",
        ),
      ),

    defconst_form: $ =>
      prec(
        1,
        seq(
          "(",
          field("operator", alias("defconst", $.symbol)),
          field("name", choice($.symbol, $.qualified_symbol, $.quote)),
          field("value", $._expression),
          optional(field("docstring", $.string)),
          ")",
        ),
      ),

    // --- Supporting rules ---

    formals: $ =>
      seq(
        "(",
        repeat(
          choice(
            $.symbol,
            $.qualified_symbol,
          ),
        ),
        ")",
      ),

    let_bindings: $ =>
      choice(
        // Bracket-delimited: [(x 1) (y 2)]
        seq(
          "[",
          repeat($.let_binding),
          "]",
        ),
        // Paren-delimited: ((x 1) (y 2))
        seq(
          "(",
          repeat($.let_binding),
          ")",
        ),
      ),

    let_binding: $ =>
      choice(
        // Bracket form: [name value] or [name (formals) body...] (labels/flet)
        seq("[", $._expression, repeat1($._expression), "]"),
        // Paren form: (name value) or (name (formals) body...) (labels/flet)
        seq("(", $._expression, repeat1($._expression), ")"),
      ),

    // --- Extras ---

    comment: _ => token(seq(";", /[^\n]*/)),
  },
});
