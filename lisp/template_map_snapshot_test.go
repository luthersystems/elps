// Copyright © 2026 The ELPS authors

package lisp

import (
	"strings"
	"testing"
)

// snapshotPayload is a struct value, so admission has to ask the native policy
// about it: no scalar kind and no immutability marker.
type snapshotPayload struct{ name string }

// Admission runs embedder policy callbacks while it walks, and a callback can
// hold the source environment. Both map arms used to read the entry's value
// out of the backing at the moment the walk reached it, so a callback that
// deleted a later key removed a value from the walk without removing it from
// the map -- and for a JSON-map backing the deleted key's type assertion ran
// on a nil interface and panicked out of NewTemplate.
//
// Entries are now taken when the walk ENTERS the container, which is the
// copier's contract, so what a callback does to the map cannot change what is
// admitted.
func TestTemplateAdmissionSnapshotsMapEntries(t *testing.T) {
	for _, backing := range []string{"sorted-map", "json-map"} {
		t.Run(backing, func(t *testing.T) {
			env := NewEnv(nil)
			trigger := Native(snapshotPayload{name: "trigger"})
			rejected := Native(snapshotPayload{name: "rejected"})
			// "a-" sorts before "z-", so the trigger is admitted (and its
			// callback runs) before the walk reaches the rejected entry.
			var m *LVal
			var del func()
			switch backing {
			case "sorted-map":
				m = SortedMap()
				m.MapSetString("a-trigger", trigger)
				m.MapSetString("z-rejected", rejected)
				del = func() { m.Map().Del(String("z-rejected")) }
			case "json-map":
				raw := jsonMap{"a-trigger": trigger, "z-rejected": rejected}
				m = SortedMapFromData(NewMapData(raw))
				del = func() { delete(raw, "z-rejected") }
			}
			env.scope = scopeOf(map[string]*LVal{"m": m})

			calls := 0
			policy := func(payload any) bool {
				p, ok := payload.(snapshotPayload)
				if !ok {
					return false
				}
				calls++
				if p.name == "trigger" {
					del()
					return true
				}
				return false
			}

			_, err := NewTemplate(env, TemplateWithNativePolicy(policy))
			if err == nil {
				t.Fatalf("publication admitted a payload the walk never inspected (policy calls: %d)", calls)
			}
			if !strings.Contains(err.Error(), "no template immutability declaration") {
				t.Fatalf("unexpected rejection: %v", err)
			}
			if calls != 2 {
				t.Errorf("policy saw %d payloads, want both entries the map held on entry", calls)
			}
		})
	}
}
