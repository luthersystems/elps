// Copyright © 2018 The ELPS authors

package token

import "testing"

func TestTypeString(t *testing.T) {
	used := make(map[string]bool)
	for tok := Type(0); tok < numTokenTypes; tok++ {
		str := tok.String()
		t.Log(str)
		if str == "" {
			t.Errorf("token type %x has empty string value", tok)
			continue
		}
		if used[str] {
			t.Errorf("token type string used twice: %v", tok)
		}
		used[str] = true
	}
}
