// Copyright © 2024 The ELPS authors

package diagnostic

import (
	"os"
)

// ColorMode controls when ANSI color codes are used.
type ColorMode int

const (
	ColorAuto   ColorMode = iota // detect based on terminal and NO_COLOR
	ColorAlways                  // always use colors
	ColorNever                   // never use colors
)

// palette holds the ANSI escape sequences for diagnostic output.
type palette struct {
	bold      string
	red       string
	yellow    string
	blue      string
	cyan      string
	boldRed   string
	boldBlue  string
	boldCyan  string
	reset     string
}

var ansiPalette = palette{
	bold:      "\033[1m",
	red:       "\033[31m",
	yellow:    "\033[33m",
	blue:      "\033[34m",
	cyan:      "\033[36m",
	boldRed:   "\033[1;31m",
	boldBlue:  "\033[1;34m",
	boldCyan:  "\033[1;36m",
	reset:     "\033[0m",
}

var noPalette = palette{}

// choosePalette selects the appropriate color palette based on the mode
// and the output file descriptor.
func choosePalette(mode ColorMode, w *os.File) palette {
	switch mode {
	case ColorAlways:
		return ansiPalette
	case ColorNever:
		return noPalette
	default: // ColorAuto
		if os.Getenv("NO_COLOR") != "" {
			return noPalette
		}
		if !isTerminal(w) {
			return noPalette
		}
		return ansiPalette
	}
}

// isTerminal reports whether f is connected to a terminal.
func isTerminal(f *os.File) bool {
	if f == nil {
		return false
	}
	fi, err := f.Stat()
	if err != nil {
		return false
	}
	return fi.Mode()&os.ModeCharDevice != 0
}
