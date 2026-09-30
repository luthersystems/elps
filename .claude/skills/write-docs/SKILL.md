# /write-docs: House Writing Style

Use this style for all text you write: docs, docstrings, code comments, PR
titles and bodies, issue text, release notes and commit messages.

## Trigger

Use before you write or edit any text that a person will read.

## Rules

Write in the spirit of Simplified Technical English (ASD-STE100).

1. Put the answer or the rule first. Put the details after it.
2. Keep sentences short: 20 words or fewer.
3. Use active voice and present tense.
4. Give each word one meaning. Use the same word for the same thing.
5. Write one instruction per sentence.
6. Do not use idioms, filler or marketing adjectives. Filler includes "note
   that", "it's worth noting", "simply" and "basically".
7. Do not use em dashes. Use a period, comma, colon or parentheses.
8. Do not use emoji.
9. Be specific. Give exact names, units, numbers and versions. Cite code
   as `path:line`.
10. Write for the reader's next action, not for the system's internals.
11. Describe what is true now. Do not tell the history of unreleased work
    (drafts, earlier designs, "this replaced X, which never shipped"). A
    short "alternatives considered" note is allowed only when it explains
    a current design choice.
12. Never name a customer. Never include customer code or data.
13. Link GitHub items as a full markdown link or as `owner/repo#N`. Never
    write a bare `#N`.
14. Use a table for comparisons, options and status. Use a list only for
    parallel items.

## Do / Don't

| Don't | Do |
|-------|----|
| `set!` will basically throw an error if the symbol isn't there. | `set!` returns an error when the symbol is unbound. |
| Fix #742 | Fix luthersystems/elps#742 |
| The parser was rewritten — it used to be a PEG parser that we dropped. | `parser.NewReader` returns an `rdparser` reader. |
| This blazing-fast new builtin makes JSON a breeze! | `json:load-bytes` parses JSON bytes. JSON objects become sorted-maps. |
| Note that the step budget can be exceeded in some cases. | Exhausting the step budget raises `step-budget-exceeded`. |
| See the lexer for details. | See `parser/lexer/lexer.go:120`. |

## Self-check

Run this check on the text before you submit it:

- [ ] The first sentence gives the answer or the rule.
- [ ] No sentence is longer than about 20 words.
- [ ] No em dashes, emoji, filler or marketing adjectives.
- [ ] Every GitHub reference is a link or `owner/repo#N`.
- [ ] Names, numbers, versions and paths are exact.
- [ ] No history of unreleased work.
- [ ] No customer names, code or data.
- [ ] A reader knows what to do next.
