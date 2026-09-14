// Copyright © 2026 The ELPS authors

package repl

import (
	"context"
	"encoding/json"
	"io"
	"unicode/utf8"
)

const jsonOutputChunk = 1024
const jsonTruncatedTail = ",\"truncated\":true}\n"

type jsonField struct {
	name  string
	parts []string
}

// jsonLineOutput budgets encoded bytes, including escapes, keys, punctuation,
// and the newline. Writes end at JSON string token boundaries so cancellation
// can close the current string and object in one bounded cleanup write.
type jsonLineOutput struct {
	ctx       context.Context
	w         io.Writer
	pending   []byte
	remaining int
	failed    bool
}

func (o *jsonLineOutput) cancelled() bool { return o.ctx != nil && o.ctx.Err() != nil }

func (o *jsonLineOutput) append(s string) {
	o.pending = append(o.pending, s...)
	o.remaining -= len(s)
}

func (o *jsonLineOutput) flush() {
	if o.failed || len(o.pending) == 0 {
		return
	}
	n, err := o.w.Write(o.pending)
	o.failed = err != nil || n != len(o.pending)
	o.pending = o.pending[:0]
}

// emitJSONLine streams string fields without ever marshaling an unbounded
// string. The reserved tail keeps truncation and cancellation valid JSON.
// Limits below four bytes cannot hold even the minimal marker line, so emit
// nothing; small limits otherwise use the JSON string "~" as the marker.
func emitJSONLine(ctx context.Context, w io.Writer, limit int, fields ...jsonField) {
	o := jsonLineOutput{ctx: ctx, w: w, remaining: limit}
	if limit < len("{\"truncated\":true}\n") {
		if limit >= len("\"~\"\n") {
			o.append("\"~\"\n")
			o.flush()
		}
		return
	}
	o.append("{")
	truncated := false
	count := 0
	for _, field := range fields {
		// Field names are fixed internal constants, never user text.
		prefix := "\"" + field.name + "\":\""
		if count > 0 {
			prefix = "," + prefix
		}
		if o.cancelled() || o.remaining < len(prefix)+1+len(jsonTruncatedTail) {
			truncated = true
			break
		}
		o.append(prefix)
		count++
		for _, part := range field.parts {
			for len(part) > 0 {
				if o.cancelled() || o.failed {
					truncated = true
					break
				}
				n := min(len(part), jsonOutputChunk)
				// Do not split a valid multi-byte rune across Marshal calls.
				for n < len(part) && n > 0 && !utf8.RuneStart(part[n]) {
					n--
				}
				if n == 0 { // invalid UTF-8 continuation bytes
					n = min(len(part), jsonOutputChunk)
				}
				data, _ := json.Marshal(part[:n]) // strings always marshal successfully
				encoded := data[1 : len(data)-1]
				available := o.remaining - 1 - len(jsonTruncatedTail)
				end := jsonStringPrefix(encoded, available)
				o.append(string(encoded[:end]))
				if end != len(encoded) {
					truncated = true
					break
				}
				part = part[n:]
				if len(o.pending) >= jsonOutputChunk {
					if o.cancelled() {
						truncated = true
						break
					}
					o.flush()
				}
			}
			if truncated {
				break
			}
		}
		o.append("\"")
		if truncated || o.failed {
			break
		}
	}
	if truncated || o.cancelled() {
		if count == 0 {
			o.append(jsonTruncatedTail[1:])
		} else {
			o.append(jsonTruncatedTail)
		}
	} else {
		o.append("}\n")
	}
	// Cancellation permits only this bounded syntax-closing write. A generic
	// io.Writer cannot interrupt a Write already in progress; never spawn a
	// goroutine that could continue writing after the request has returned.
	o.flush()
}

// jsonStringPrefix fits complete escape sequences and UTF-8 runes into limit.
// Its input is one bounded, already encoded JSON string (without quotes).
func jsonStringPrefix(s []byte, limit int) int {
	i := 0
	for i < len(s) {
		n := 1
		if s[i] == '\\' {
			n = 2
			if s[i+1] == 'u' {
				n = 6
			}
		} else if s[i] >= utf8.RuneSelf {
			_, n = utf8.DecodeRune(s[i:])
		}
		if n > limit-i {
			break
		}
		i += n
	}
	return i
}
