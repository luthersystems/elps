// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"context"
	"os"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// These tests cover issue #325: s:may-have-key looked its key up as a SYMBOL
// while s:has-key, s:when and s:no-other-keys looked the same key up as a
// STRING.
//
// WHY THAT MATTERS AT ALL, given lisp.Symbol("a") and lisp.String("a") carry
// the same Str: lisp.Map is an INTERFACE, and its implementations disagree
// about key types.
//
//   - lisp/maps.go sortedmap keys on LVal.Str for both LString and LSymbol.
//     A symbol lookup and a string lookup are the same lookup, and either
//     finds an entry stored under the other. The asymmetry is INVISIBLE here,
//     which is why the literal (sorted-map "a" 1) named in #325 was in fact
//     never affected -- worth recording, because it is the case a reader
//     reaches for first when checking the report.
//   - libjson.SortedMap -- what json:load-string returns -- rejects any key
//     whose Type is not LString and answers (LError, false).
//
// may-have-key read that false as "the key is absent" and passed. So against
// a JSON-decoded map -- the case the constraint is most often written for --
// it was a total no-op: an optional key holding a wrong-typed value validated
// clean. A wrong answer rather than a crash, which is why no fuzz target
// found it.
//
// The tests below pin the fixed behaviour across all three map flavours and
// all four key-taking constraints, and pin the two sites where an error from
// may-have-key is INVERTED rather than propagated.

// value is one payload written two ways, because a JSON-decoded map has to be
// built from JSON source rather than from ELPS literals.
type value struct{ name, elps, json string }

var (
	vString = value{"string", `"str"`, `"str"`}
	vNumber = value{"number", `1`, `1`}
	vBool   = value{"bool", `true`, `true`}
	vMap    = value{"map", `(sorted-map "n" 1)`, `{"n": 1}`}
	vYes    = value{"yes", `"yes"`, `"yes"`}
	vNo     = value{"no", `"no"`, `"no"`}
)

// mapFlavour is one way of building a sorted-map that reaches a constraint.
// The three of them exercise the three distinct key/Map behaviours described
// above; a constraint must not be able to tell them apart.
type mapFlavour struct {
	name string
	// expr builds a sorted-map with key "a" bound to v, plus an unrelated
	// numeric key "z" so the map is never empty.
	expr func(v value) string
	// absent builds the same shape with no "a" key at all.
	absent string
}

var mapFlavours = []mapFlavour{
	{
		name:   "native-string-keys",
		expr:   func(v value) string { return `(sorted-map "a" ` + v.elps + ` "z" 0)` },
		absent: `(sorted-map "z" 0)`,
	},
	{
		// A literal symbol key. The built-in map stores it under the same Str,
		// so a string lookup must still find it -- this is the half of the
		// contract that a "just use symbols everywhere instead" fix would
		// have kept, and it must survive the string decision too.
		name:   "native-symbol-keys",
		expr:   func(v value) string { return `(sorted-map 'a ` + v.elps + ` 'z 0)` },
		absent: `(sorted-map 'z 0)`,
	},
	{
		// The flavour #325 is actually about.
		name:   "json-decoded",
		expr:   func(v value) string { return jsonMap(`"a": ` + v.json + `, "z": 0`) },
		absent: jsonMap(`"z": 0`),
	},
}

// jsonMap wraps JSON object body in a json:load-string call, escaping it for
// the ELPS string literal it lands in.
func jsonMap(body string) string {
	return `(json:load-string "{` + strings.ReplaceAll(body, `"`, `\"`) + `}")`
}

// validates reports whether src validated cleanly, failing the test if it
// crashed rather than answering.
func validates(t *testing.T, name, src string) bool {
	t.Helper()
	env := newSchemaEnv(t)
	res := env.LoadStringContext(context.Background(), name, src)
	if lisp.IsInternalPanic(res) {
		t.Fatalf("%s panicked: %v\n--- source ---\n%s", name, res, src)
	}
	return res.Type != lisp.LError
}

// TestKeyConstraintsAgreeAcrossMapImplementations is the headline test: every
// key-taking constraint must give the same answer for the same logical map,
// whichever implementation is carrying it.
//
// Before the fix the may-have-key/json-decoded/present-wrong-type row
// returned "valid" -- the silent no-op.
func TestKeyConstraintsAgreeAcrossMapImplementations(t *testing.T) {
	cases := []struct {
		name string
		// def is a constraint expression placed in a sorted-map deftype.
		def string
		// at is the value bound to key "a", or nil to omit the key.
		at    *value
		valid bool
	}{
		// s:has-key -- the constraint that was already right, kept here as
		// the control the other three are compared against.
		{"has-key/present-ok", `(s:has-key "a" s:string)`, &vString, true},
		{"has-key/present-wrong-type", `(s:has-key "a" s:string)`, &vNumber, false},
		{"has-key/absent", `(s:has-key "a" s:string)`, nil, false},

		// s:may-have-key -- #325.
		{"may-have-key/present-ok", `(s:may-have-key "a" s:string)`, &vString, true},
		{"may-have-key/present-wrong-type", `(s:may-have-key "a" s:string)`, &vNumber, false},
		{"may-have-key/absent", `(s:may-have-key "a" s:string)`, nil, true},

		// s:when reads the value at a key too.
		{"when/guard-met-check-fails", `(s:when "a" (s:in "yes") "z" (s:gt 100))`, &vYes, false},
		{"when/guard-met-check-passes", `(s:when "a" (s:in "yes") "z" (s:lt 100))`, &vYes, true},
		{"when/guard-not-met", `(s:when "a" (s:in "yes") "z" (s:gt 100))`, &vNo, true},
		{"when/guard-key-absent", `(s:when "a" (s:in "yes") "z" (s:gt 100))`, nil, true},

		// s:no-other-keys consumes the key names the two key constraints
		// return, so it is downstream of the lookup as well.
		{"no-other-keys/declared", `(s:no-other-keys (s:may-have-key "a" s:string) (s:may-have-key "z" s:number))`, &vString, true},
		{"no-other-keys/declared-wrong-type", `(s:no-other-keys (s:may-have-key "a" s:string) (s:may-have-key "z" s:number))`, &vNumber, false},
		{"no-other-keys/undeclared", `(s:no-other-keys (s:may-have-key "z" s:number))`, &vString, false},
	}

	for _, f := range mapFlavours {
		for _, c := range cases {
			t.Run(f.name+"/"+c.name, func(t *testing.T) {
				m := f.absent
				if c.at != nil {
					m = f.expr(*c.at)
				}
				src := `(s:deftype "T" s:sorted-map ` + c.def + `)
					(s:validate T ` + m + `)`
				if got := validates(t, c.name, src); got != c.valid {
					t.Fatalf("expected valid=%v, got %v\n--- source ---\n%s",
						c.valid, got, src)
				}
			})
		}
	}
}

// TestMayHaveKeyAgreesWithHasKeyOnPresence states the invariant directly
// rather than by enumeration: on a map that HAS the key, may-have-key and
// has-key must return the same verdict. They may differ only when the key is
// absent, which is the whole of may-have-key's job.
//
// This is the assertion that would have failed for every JSON map before the
// fix, and it keeps failing if a future change reintroduces the divergence in
// either direction.
func TestMayHaveKeyAgreesWithHasKeyOnPresence(t *testing.T) {
	for _, f := range mapFlavours {
		for _, v := range []value{vString, vNumber, vBool, vMap} {
			t.Run(f.name+"/"+v.name, func(t *testing.T) {
				m := f.expr(v)
				has := validates(t, "has", `(s:deftype "T" s:sorted-map (s:has-key "a" s:string))
					(s:validate T `+m+`)`)
				may := validates(t, "may", `(s:deftype "T" s:sorted-map (s:may-have-key "a" s:string))
					(s:validate T `+m+`)`)
				if has != may {
					t.Fatalf("key is present, so s:has-key and s:may-have-key must agree: "+
						"has-key valid=%v, may-have-key valid=%v, map %s.\n"+
						"A may-have-key that passes where has-key fails is looking the key "+
						"up differently -- see issue #325.", has, may, m)
				}
			})
		}
	}
}

// TestMayHaveKeyInversionSites is the B3a check, and it is here because the
// #320 post-mortem records a libschema hardening fix that turned a loud crash
// into a SILENTLY PASSING validation. Making may-have-key start returning
// errors where it previously always succeeded is exactly the kind of change
// that can invert somewhere downstream, so both inverting consumers are
// pinned rather than reasoned about.
//
// The two sites that do NOT propagate an error from a constraint:
//
//   - s:not, which reads an error as "the inner constraint failed, so I pass".
//   - s:when's guard slot, which reads an error as "the condition is not met,
//     skip this clause".
//
// Neither is a defect introduced here. In both, the error may-have-key now
// produces is a genuine DOMAIN answer ("the value at this key does not match")
// and inverting a domain answer is what those combinators are for. That is the
// distinction #320 B3a turned on: there the inverted error was an
// INFRASTRUCTURE answer ("this is not a constraint at all"), which is why it
// is rejected at construction and can never reach an inverting caller.
//
// What did change is the outcome for JSON maps, where may-have-key used to be
// a constant success:
//
//	(s:not (s:may-have-key ...))     always FAILED  -> now data-dependent
//	(s:when k (s:may-have-key ...))  always ran     -> now can skip
//
// A schema cannot meaningfully have depended on either, since neither looked
// at the data, but the new answers are recorded here so the next change to
// this area has to notice them.
func TestMayHaveKeyInversionSites(t *testing.T) {
	cases := []struct {
		name  string
		src   string
		valid bool
	}{
		// s:not around may-have-key. The inner constraint is applied to the
		// map itself.
		{
			"not/inner-passes-so-not-fails",
			`(s:deftype "T" s:sorted-map (s:not (s:may-have-key "a" s:string)))
			 (s:validate T (json:load-string "{\"a\": \"str\"}"))`,
			false,
		},
		{
			"not/inner-fails-so-not-passes",
			`(s:deftype "T" s:sorted-map (s:not (s:may-have-key "a" s:string)))
			 (s:validate T (json:load-string "{\"a\": 1}"))`,
			true,
		},
		{
			"not/key-absent-so-inner-passes-so-not-fails",
			`(s:deftype "T" s:sorted-map (s:not (s:may-have-key "a" s:string)))
			 (s:validate T (json:load-string "{\"z\": 0}"))`,
			false,
		},

		// may-have-key in s:when's guard slot. The guard is applied to the
		// VALUE at the key, so the value has to be a map itself.
		{
			"when-guard/guard-satisfied-clause-runs-and-fails",
			`(s:deftype "T" s:sorted-map (s:when "sub" (s:may-have-key "a" s:string) "n" (s:gt 100)))
			 (s:validate T (json:load-string "{\"sub\": {\"a\": \"str\"}, \"n\": 1}"))`,
			false,
		},
		{
			"when-guard/guard-unsatisfied-clause-skipped",
			`(s:deftype "T" s:sorted-map (s:when "sub" (s:may-have-key "a" s:string) "n" (s:gt 100)))
			 (s:validate T (json:load-string "{\"sub\": {\"a\": 1}, \"n\": 1}"))`,
			true,
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := validates(t, c.name, c.src); got != c.valid {
				t.Fatalf("expected valid=%v, got %v\n--- source ---\n%s", c.valid, got, c.src)
			}
		})
	}
}

// strictKeyMap is a lisp.Map that refuses EVERY key, the way
// libjson.SortedMap refuses non-string keys: it answers (LError, false).
//
// It stands in for a future Map implementation stricter than anything in the
// tree today. The point is the shape of the answer, not the strictness: it
// proves may-have-key distinguishes Get's two falses instead of folding
// "cannot search this map" into "key is absent, so pass" -- the fold that
// made #325 silent.
type strictKeyMap struct{}

func (strictKeyMap) Len() int { return 0 }

func (strictKeyMap) Get(k *lisp.LVal) (*lisp.LVal, bool) {
	return lisp.Errorf("this map cannot be searched by %s keys", lisp.GetType(k)), false
}

func (strictKeyMap) Set(k, v *lisp.LVal) *lisp.LVal {
	return lisp.Errorf("this map cannot be searched by %s keys", lisp.GetType(k))
}

func (strictKeyMap) Del(k *lisp.LVal) *lisp.LVal {
	return lisp.Errorf("this map cannot be searched by %s keys", lisp.GetType(k))
}

func (strictKeyMap) Keys() *lisp.LVal { return lisp.QExpr(nil) }

func (strictKeyMap) Entries([]*lisp.LVal) *lisp.LVal { return lisp.Int(0) }

var _ lisp.Map = strictKeyMap{}

// TestMayHaveKeyCannotSilentlyPassOnAnUnsearchableMap closes the CLASS rather
// than the instance. Swapping Symbol for String fixes libjson today; it does
// not stop the next Map implementation from rejecting whatever key libschema
// hands it and putting may-have-key straight back into "always passes".
func TestMayHaveKeyCannotSilentlyPassOnAnUnsearchableMap(t *testing.T) {
	env := newSchemaEnv(t)
	m := lisp.SortedMapFromData(lisp.NewMapData(strictKeyMap{}))
	if rc := env.PutGlobal(lisp.Symbol("hostile"), m); rc.Type == lisp.LError {
		t.Fatalf("bind: %v", rc)
	}
	res := env.LoadStringContext(context.Background(), "hostile",
		`(s:deftype "T" s:sorted-map (s:may-have-key "a" s:string)) (s:validate T hostile)`)
	if lisp.IsInternalPanic(res) {
		t.Fatalf("panicked instead of answering: %v", res)
	}
	if res.Type != lisp.LError {
		t.Fatalf("s:may-have-key PASSED against a map that cannot be searched at all (%v).\n"+
			"'Get said not-found' and 'Get said it cannot answer' are different, and "+
			"folding the second into the first is what made issue #325 invisible.", res)
	}

	// Control: s:has-key on the same map already fails, so the test above is
	// not merely observing that everything fails on a hostile map.
	env2 := newSchemaEnv(t)
	if rc := env2.PutGlobal(lisp.Symbol("hostile"),
		lisp.SortedMapFromData(lisp.NewMapData(strictKeyMap{}))); rc.Type == lisp.LError {
		t.Fatalf("bind: %v", rc)
	}
	if res := env2.LoadStringContext(context.Background(), "hostile-has",
		`(s:deftype "T" s:sorted-map (s:has-key "a" s:string)) (s:validate T hostile)`); res.Type != lisp.LError {
		t.Fatalf("s:has-key unexpectedly passed on an unsearchable map: %v", res)
	}
}

// mapLookupRe matches a lisp.Map lookup. env.Get is an ENVIRONMENT lookup and
// is excluded by envLookupRe below; every other .Get( in libschema.go reaches
// a lisp.Map.
var (
	mapLookupRe = regexp.MustCompile(`\.Get\(|\bMapGet\(`)
	envLookupRe = regexp.MustCompile(`\benv\.Get\(`)
	keyedRe     = regexp.MustCompile(`\.Get\(schemaKey\(`)
)

// TestSchemaKeysAreLookedUpAsStrings is the drift guard.
//
// #325 was one call site out of four disagreeing with the other three about
// the LVal type of a map key -- a difference invisible on the built-in map
// and fatal on a strict one, so neither review nor fuzzing caught it. Fixing
// the one site leaves the class wide open: the next person to add a key-taking
// constraint has no reason to know the key type is load-bearing.
//
// Every map lookup therefore goes through schemaKey, and this test fails if
// one does not. MapGet is banned outright because its *LVal overload takes
// whatever key LVal it is handed.
func TestSchemaKeysAreLookedUpAsStrings(t *testing.T) {
	src, err := os.ReadFile("libschema.go")
	if err != nil {
		t.Fatalf("read libschema.go: %v", err)
	}
	var offenders []string
	lookups := 0
	for i, line := range strings.Split(string(src), "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "//") {
			continue // a doc comment naming Get is not a call site
		}
		if !mapLookupRe.MatchString(line) || envLookupRe.MatchString(line) {
			continue
		}
		lookups++
		if !keyedRe.MatchString(line) {
			offenders = append(offenders, "libschema.go:"+strconv.Itoa(i+1)+": "+trimmed)
		}
	}
	if lookups == 0 {
		t.Fatal("found no sorted-map lookups in libschema.go at all -- this guard has " +
			"gone vacuous, which means it is no longer guarding anything")
	}
	if len(offenders) != 0 {
		t.Fatalf("sorted-map lookups that do not go through schemaKey:\n  %s\n\n"+
			"lisp.Map implementations disagree about key types: the built-in "+
			"sortedmap accepts LString and LSymbol interchangeably, libjson.SortedMap "+
			"accepts only LString and answers (LError, false) for anything else. A "+
			"lookup with the wrong key type therefore reports 'not found' instead of "+
			"failing, and in s:may-have-key that read as 'absent, so pass' -- a "+
			"validator that silently approved everything. See issue #325.",
			strings.Join(offenders, "\n  "))
	}
}
