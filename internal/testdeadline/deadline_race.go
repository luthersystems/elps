//go:build race

package testdeadline

// The race detector costs roughly 10x in CPU and more in memory; 12 leaves
// room for a loaded CI runner without making a real hang wait long.
const factor = 12
