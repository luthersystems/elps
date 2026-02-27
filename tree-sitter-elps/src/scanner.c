#include "tree_sitter/parser.h"
#include <stdbool.h>

// External token types — must match the order in grammar.js externals
enum TokenType {
  RAW_STRING,
  STRING,
};

void *tree_sitter_elps_external_scanner_create(void) { return NULL; }

void tree_sitter_elps_external_scanner_destroy(void *payload) {}

unsigned tree_sitter_elps_external_scanner_serialize(void *payload,
                                                     char *buffer) {
  return 0;
}

void tree_sitter_elps_external_scanner_deserialize(void *payload,
                                                   const char *buffer,
                                                   unsigned length) {}

// Scan for strings and raw strings.
// Both start with " so the external scanner must disambiguate:
//   """..."""  → raw_string
//   "..."     → string (including "" empty string)
//
// This must be external because tree-sitter's internal tokenizer would
// greedily match "" as an empty string, preventing recognition of """.
//
// The scanner skips whitespace before looking for the opening quote because
// tree-sitter only calls the external scanner once per lex position — the
// internal lexer's SKIP for whitespace does not re-invoke the external scanner.
bool tree_sitter_elps_external_scanner_scan(void *payload, TSLexer *lexer,
                                            const bool *valid_symbols) {
  if (!valid_symbols[RAW_STRING] && !valid_symbols[STRING]) {
    return false;
  }

  // Skip whitespace and comments — the external scanner must do this itself
  // because tree-sitter's internal lexer whitespace SKIP does not re-trigger
  // the external scanner. Comments (;...\n) are also extras in the grammar.
  while (!lexer->eof(lexer)) {
    if (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
        lexer->lookahead == '\r' || lexer->lookahead == '\n') {
      lexer->advance(lexer, true);
    } else if (lexer->lookahead == ';') {
      // Skip comment to end of line
      while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
        lexer->advance(lexer, true);
      }
    } else {
      break;
    }
  }

  if (lexer->lookahead != '"') {
    return false;
  }

  // Consume the opening "
  lexer->advance(lexer, false);

  if (lexer->lookahead != '"') {
    // Regular string: "content..."
    if (!valid_symbols[STRING]) return false;
    while (!lexer->eof(lexer)) {
      if (lexer->lookahead == '\\') {
        lexer->advance(lexer, false);
        if (lexer->eof(lexer)) return false;
        lexer->advance(lexer, false);
      } else if (lexer->lookahead == '"') {
        lexer->advance(lexer, false);
        lexer->mark_end(lexer);
        lexer->result_symbol = STRING;
        return true;
      } else if (lexer->lookahead == '\n') {
        return false; // unterminated string
      } else {
        lexer->advance(lexer, false);
      }
    }
    return false; // unterminated
  }

  // We've seen "" — check for third "
  lexer->advance(lexer, false);

  if (lexer->lookahead != '"') {
    // Just "" — empty string
    if (!valid_symbols[STRING]) return false;
    lexer->mark_end(lexer);
    lexer->result_symbol = STRING;
    return true;
  }

  // We've seen """ — raw string
  if (!valid_symbols[RAW_STRING]) return false;
  lexer->advance(lexer, false);

  // Consume content until closing """
  int quote_count = 0;
  while (!lexer->eof(lexer)) {
    if (lexer->lookahead == '"') {
      quote_count++;
      lexer->advance(lexer, false);
      if (quote_count == 3) {
        lexer->mark_end(lexer);
        lexer->result_symbol = RAW_STRING;
        return true;
      }
    } else {
      quote_count = 0;
      lexer->advance(lexer, false);
    }
  }

  return false; // unterminated raw string
}
