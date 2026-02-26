// Copyright © 2024 The ELPS authors

// Package docs embeds the ELPS language reference for use by the CLI.
package docs

import _ "embed"

//go:embed lang.md
var LangGuide string

//go:embed debugging-guide.md
var DebuggingGuide string
