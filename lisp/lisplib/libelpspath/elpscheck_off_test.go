// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package libelpspath

// elpscheckEnabled reports whether this test binary was built with the
// elpscheck runtime checkers (-tags elpscheck).
const elpscheckEnabled = false
