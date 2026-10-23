// Copyright © 2026 The ELPS authors

package elpstag_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/elpstag"
	"github.com/stretchr/testify/assert"
)

type Embedded struct{ X int }

type tagged struct {
	Plain    int
	Named    int `elps:"named"`
	Omit     int `elps:"omit,omitempty"`
	OnlyOpt  int `elps:",omitempty"`
	Skipped  int `elps:"-"`
	hidden   int
	Embedded `elps:"embedded"`
	PLAIN    int // a duplicate of Plain, ignoring case
	Other    int `elps:"other,unknown"`
}

func TestFields(t *testing.T) {
	_ = tagged{}.hidden
	got := elpstag.Fields(reflect.TypeFor[tagged]())
	assert.Equal(t, []elpstag.Field{
		{Name: "Plain", Index: 0},
		{Name: "named", Index: 1},
		{Name: "omit", Index: 2, OmitEmpty: true},
		{Name: "OnlyOpt", Index: 3, OmitEmpty: true},
		{Name: "embedded", Index: 6},
		{Name: "other", Index: 8},
	}, got)
}

func TestFieldsEmbeddedUntagged(t *testing.T) {
	type s struct{ Embedded }
	assert.Equal(t, []elpstag.Field{{Name: "Embedded", Index: 0}},
		elpstag.Fields(reflect.TypeFor[s]()))
}

func TestMatch(t *testing.T) {
	for _, tc := range []struct {
		key, name    string
		match, exact bool
	}{
		{"name", "name", true, true},
		{":name", "name", true, true},
		{"Name", "name", true, false},
		{":NAME", "name", true, false},
		{"names", "name", false, false},
		{"::name", "name", false, false},
	} {
		m, e := elpstag.Match(tc.key, tc.name)
		assert.Equal(t, tc.match, m, tc.key)
		assert.Equal(t, tc.exact, e, tc.key)
	}
}

func TestFind(t *testing.T) {
	fields := elpstag.Fields(reflect.TypeFor[tagged]())
	i, exact := elpstag.Find(fields, "NAMED")
	assert.Equal(t, 1, i)
	assert.False(t, exact)
	i, exact = elpstag.Find(fields, ":named")
	assert.Equal(t, 1, i)
	assert.True(t, exact)
	i, _ = elpstag.Find(fields, "Skipped")
	assert.Equal(t, -1, i)
}
