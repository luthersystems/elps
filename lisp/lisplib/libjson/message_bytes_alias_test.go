// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// json:message-bytes must return bytes the caller owns.
//
// The builtin returned the message's own backing array, with the spare
// capacity the encoder's pooled buffer left behind it.  append-bytes! grows
// in place when there is capacity, so an append to one result wrote into
// every other result of the same message, and into the message itself.  How
// much spare capacity there is depends on the buffer pool, so two peers
// could compute different bytes from one program
// (luthersystems/elps#932).
func TestMessageBytesResultsDoNotShareABuffer(t *testing.T) {
	env := newLispEnv(t)
	evalIn(t, env, `(set 'm (json:dump-message "aaaaaaaa"))`)
	evalIn(t, env, `(set 'b1 (json:message-bytes m))`)
	evalIn(t, env, `(set 'b2 (json:message-bytes m))`)
	evalIn(t, env, `(append-bytes! b1 "X")`)
	evalIn(t, env, `(append-bytes! b2 "Y")`)

	assert.Equal(t, `"aaaaaaaa"X`, evalIn(t, env, `(to-string b1)`).Str)
	assert.Equal(t, `"aaaaaaaa"Y`, evalIn(t, env, `(to-string b2)`).Str)
	// The message is immutable: a write through a result does not reach it.
	evalIn(t, env, `(set 'b3 (json:message-bytes m))`)
	evalIn(t, env, `(append-bytes! b3 "Z")`)
	assert.Equal(t, `"aaaaaaaa"`, evalIn(t, env, `(json:dump-string (json:load-message m))`).Str)
	assert.Equal(t, `"aaaaaaaa"`, evalIn(t, env, `(to-string (json:message-bytes m))`).Str)
}
