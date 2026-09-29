// Command apibreak decides whether a change breaks elps's public API, and
// whether every break it finds is covered by a reviewed entry in
// scripts/api-breaks.txt (issue #761).
//
// It judges two surfaces, both computed by scripts/api-break-gate.sh from the
// PR's base and head:
//
//   - go: the incompatible-change report of golang.org/x/exp/cmd/apidiff run
//     in module mode (internal packages are ignored by apidiff itself), passed
//     with -go-report. Each "- ./pkg.Sym: what" line is one break, keyed
//     "pkg.Sym".
//
//   - lisp: two `elps doc --json -l` dumps, passed with -lisp-base and
//     -lisp-head. A removed package (key "pkg:*"), a removed export
//     ("pkg:sym"), a changed kind (function, macro, operator, variable) and a
//     formals change that rejects a call the base accepted are breaks.
//     Additions, renamed formals and docstrings are not.
//
// Exit status: 0 no unwaived break, 1 at least one unwaived break, 2 the
// input or the override file could not be interpreted. A malformed override
// file is never treated as an empty one.
//
// For every unwaived break it prints the exact line to add to the override
// file; the issue and reason fields are placeholders that do not validate, so
// the line cannot be pasted in without being filled.
package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"
	"strings"
	"time"
)

// A brk is one breaking change on one surface.
type brk struct {
	surface string // "go" or "lisp"
	symbol  string
	what    string
}

// An override is one parsed line of the override file.
type override struct {
	surface, symbol, expires, issue, reason string
	line                                    int
	expired                                 bool
	used                                    bool
}

var (
	dateRe     = regexp.MustCompile(`^[0-9]{4}-[0-9]{2}-[0-9]{2}$`)
	issueRefRe = regexp.MustCompile(`^[A-Za-z0-9._/-]*#[0-9]+$`)
	issueURLRe = regexp.MustCompile(`^https://github\.com/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/(issues|pull)/[0-9]+$`)
	fieldSplit = regexp.MustCompile(`[ \t,]+`)
	commentRe  = regexp.MustCompile(`^[ \t]*(#|$)`)
	goLineRe   = regexp.MustCompile(`^- (\S+?): (.+)$`)
)

// issuesOK is the benchmark waivers' rule (cmd/benchgate/waivers.go): one or
// more references, every token a reference, at least one present.
func issuesOK(s string) bool {
	good := 0
	for _, t := range fieldSplit.Split(s, -1) {
		if t == "" {
			continue
		}
		if !issueRefRe.MatchString(t) && !issueURLRe.MatchString(t) {
			return false
		}
		good++
	}
	return good > 0
}

// parseOverrides validates the override file with the same rules as
// scripts/benchstat-waivers.txt: five |-separated fields
// (surface | symbol | expires | issue | reason), a YYYY-MM-DD expiry, a
// parseable tracking reference and a reason of at least 10 characters.
func parseOverrides(source, content, today string) ([]*override, []string) {
	var out []*override
	var bad []string
	for i, raw := range strings.Split(content, "\n") {
		lineno := i + 1
		l := strings.TrimSuffix(raw, "\r")
		if commentRe.MatchString(l) {
			continue
		}
		report := func(msg string) {
			bad = append(bad, fmt.Sprintf("  OVERRIDE-BAD  %s:%d  %s", source, lineno, msg))
		}
		f := strings.Split(l, "|")
		if len(f) != 5 {
			report(fmt.Sprintf("expected 5 |-separated fields (surface | symbol | expires | issue | reason), found %d: %s", len(f), strings.TrimSpace(l)))
			continue
		}
		for j := range f {
			f[j] = strings.Trim(f[j], " \t")
		}
		ok := true
		if f[0] != "go" && f[0] != "lisp" {
			report(fmt.Sprintf("surface %q must be go or lisp", f[0]))
			ok = false
		}
		if f[1] == "" || strings.ContainsAny(f[1], " \t") {
			report("symbol must be the exact key the gate printed (no spaces)")
			ok = false
		}
		if !dateRe.MatchString(f[2]) {
			report(fmt.Sprintf("expires %q is not a YYYY-MM-DD date; an override with no end date is never revisited", f[2]))
			ok = false
		} else if _, err := time.Parse(time.DateOnly, f[2]); err != nil {
			report(fmt.Sprintf("expires %q is not a real date", f[2]))
			ok = false
		}
		if !issuesOK(f[3]) {
			report(fmt.Sprintf("issue %q is not a tracking reference (elps#412, substrate#392, #412, owner/repo#412 or a github.com issue/PR URL)", f[3]))
			ok = false
		}
		if len(f[4]) < 10 {
			report("reason is missing or too short; say why the break is intended and how consumers migrate")
			ok = false
		}
		if !ok {
			continue
		}
		out = append(out, &override{
			surface: f[0], symbol: f[1], expires: f[2], issue: f[3], reason: f[4],
			line: lineno, expired: today > f[2],
		})
	}
	return out, bad
}

// parseGoReport reads apidiff -incompatible output. Lines that are not change
// items ("Incompatible changes:", "Ignoring internal package ...") are
// skipped; any other non-blank line is an error, so a format change in
// apidiff cannot silently read as "no breaks".
func parseGoReport(r io.Reader) ([]brk, error) {
	var out []brk
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 1<<20), 1<<20)
	for sc.Scan() {
		l := strings.TrimRight(sc.Text(), " \r")
		switch {
		case l == "", l == "Incompatible changes:", strings.HasPrefix(l, "Ignoring internal package "):
			continue
		}
		m := goLineRe.FindStringSubmatch(l)
		if m == nil {
			return nil, fmt.Errorf("unrecognized apidiff line: %q", l)
		}
		out = append(out, brk{surface: "go", symbol: strings.TrimPrefix(m[1], "./"), what: m[2]})
	}
	return out, sc.Err()
}

// The subset of libhelp's JSON (lisp/lisplib/libhelp.PackageDoc) the gate
// reads.
type formalsDoc struct {
	Required []string `json:"required"`
	Optional []string `json:"optional"`
	Rest     string   `json:"rest"`
	Keys     []string `json:"keys"`
}

type symbolDoc struct {
	Formals *formalsDoc `json:"formals"`
	Name    string      `json:"name"`
	Kind    string      `json:"kind"`
}

type packageDoc struct {
	Name    string      `json:"name"`
	Symbols []symbolDoc `json:"symbols"`
}

func readLisp(path string) (map[string]map[string]symbolDoc, error) {
	b, err := os.ReadFile(path) //#nosec G304 -- apibreak is a CLI given the dump paths to read
	if err != nil {
		return nil, err
	}
	var pkgs []packageDoc
	if err := json.Unmarshal(b, &pkgs); err != nil {
		return nil, fmt.Errorf("%s: %w", path, err)
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("%s: no packages; refusing to compare an empty surface", path)
	}
	out := make(map[string]map[string]symbolDoc, len(pkgs))
	for _, p := range pkgs {
		m := make(map[string]symbolDoc, len(p.Symbols))
		for _, s := range p.Symbols {
			m[s.Name] = s
		}
		out[p.Name] = m
	}
	return out, nil
}

func renderFormals(f *formalsDoc) string {
	if f == nil {
		return "()"
	}
	parts := append([]string{}, f.Required...)
	if len(f.Optional) > 0 {
		parts = append(append(parts, "&optional"), f.Optional...)
	}
	if f.Rest != "" {
		parts = append(parts, "&rest", f.Rest)
	}
	if len(f.Keys) > 0 {
		parts = append(append(parts, "&key"), f.Keys...)
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// formalsCompatible reports whether every call the base formals accept is
// also accepted by the head formals. Parameters are positional, so a rename
// is compatible; keywords are matched by name.
func formalsCompatible(base, head *formalsDoc) bool {
	if base == nil || head == nil {
		return base == nil && head == nil
	}
	if len(head.Required) > len(base.Required) {
		return false
	}
	if base.Rest != "" {
		if head.Rest == "" {
			return false
		}
	} else if head.Rest == "" && len(head.Required)+len(head.Optional) < len(base.Required)+len(base.Optional) {
		return false
	}
	have := make(map[string]bool, len(head.Keys))
	for _, k := range head.Keys {
		have[k] = true
	}
	for _, k := range base.Keys {
		if !have[k] {
			return false
		}
	}
	return true
}

func lispBreaks(base, head map[string]map[string]symbolDoc) []brk {
	var out []brk
	for pkg, bsyms := range base {
		hsyms, ok := head[pkg]
		if !ok {
			out = append(out, brk{"lisp", pkg + ":*", "package removed"})
			continue
		}
		for name, b := range bsyms {
			key := pkg + ":" + name
			h, ok := hsyms[name]
			switch {
			case !ok:
				out = append(out, brk{"lisp", key, "removed"})
			case h.Kind != b.Kind:
				out = append(out, brk{"lisp", key, fmt.Sprintf("kind changed from %s to %s", b.Kind, h.Kind)})
			case !formalsCompatible(b.Formals, h.Formals):
				out = append(out, brk{"lisp", key, fmt.Sprintf("formals changed from %s to %s", renderFormals(b.Formals), renderFormals(h.Formals))})
			}
		}
	}
	return out
}

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("apibreak", flag.ContinueOnError)
	fs.SetOutput(stderr)
	overridesPath := fs.String("overrides", "", "override file (required)")
	goReport := fs.String("go-report", "", "apidiff -incompatible output")
	lispBase := fs.String("lisp-base", "", "`elps doc --json -l` output from the base")
	lispHead := fs.String("lisp-head", "", "`elps doc --json -l` output from the head")
	today := fs.String("today", time.Now().UTC().Format(time.DateOnly), "date expiry is judged against (YYYY-MM-DD)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	fail := func(format string, a ...any) int {
		_, _ = fmt.Fprintf(stderr, "apibreak: "+format+"\n", a...)
		return 2
	}
	if *overridesPath == "" || !dateRe.MatchString(*today) {
		return fail("-overrides is required and -today must be YYYY-MM-DD")
	}
	if (*lispBase == "") != (*lispHead == "") {
		return fail("-lisp-base and -lisp-head go together")
	}
	if *goReport == "" && *lispBase == "" {
		return fail("nothing to judge: pass -go-report and/or -lisp-base/-lisp-head")
	}

	content, err := os.ReadFile(*overridesPath) //#nosec G304 -- apibreak is a CLI given the override-file path to read
	if err != nil {
		return fail("%v", err)
	}
	ovs, bad := parseOverrides(*overridesPath, string(content), *today)
	if len(bad) > 0 {
		_, _ = fmt.Fprintln(stderr, strings.Join(bad, "\n"))
		return fail("%d malformed override(s); fix them, a malformed override file is never read as empty", len(bad))
	}

	var breaks []brk
	if *goReport != "" {
		f, err := os.Open(*goReport) //#nosec G304 -- apibreak is a CLI given the apidiff report path to read
		if err != nil {
			return fail("%v", err)
		}
		gb, err := parseGoReport(f)
		_ = f.Close()
		if err != nil {
			return fail("%s: %v", *goReport, err)
		}
		breaks = append(breaks, gb...)
	}
	if *lispBase != "" {
		b, err := readLisp(*lispBase)
		if err != nil {
			return fail("%v", err)
		}
		h, err := readLisp(*lispHead)
		if err != nil {
			return fail("%v", err)
		}
		breaks = append(breaks, lispBreaks(b, h)...)
	}
	sort.Slice(breaks, func(i, j int) bool {
		if breaks[i].surface != breaks[j].surface {
			return breaks[i].surface < breaks[j].surface
		}
		return breaks[i].symbol < breaks[j].symbol
	})

	find := func(b brk) *override {
		for _, o := range ovs {
			if o.surface == b.surface && o.symbol == b.symbol {
				return o
			}
		}
		return nil
	}
	suggestExpiry := func() string {
		t, _ := time.Parse(time.DateOnly, *today)
		return t.AddDate(0, 3, 0).Format(time.DateOnly)
	}

	var unwaived []brk
	for _, b := range breaks {
		o := find(b)
		switch {
		case o == nil:
			unwaived = append(unwaived, b)
			_, _ = fmt.Fprintf(stdout, "BREAK         %s %s: %s\n", b.surface, b.symbol, b.what)
		case o.expired:
			o.used = true
			unwaived = append(unwaived, b)
			_, _ = fmt.Fprintf(stdout, "BREAK         %s %s: %s (override at line %d EXPIRED %s)\n", b.surface, b.symbol, b.what, o.line, o.expires)
		default:
			o.used = true
			_, _ = fmt.Fprintf(stdout, "WAIVED        %s %s: %s (%s, expires %s)\n", b.surface, b.symbol, b.what, o.issue, o.expires)
		}
	}
	for _, o := range ovs {
		if !o.used {
			// Expected after the break merges: base then equals head. The
			// entry stays until the next release so release notes list it.
			_, _ = fmt.Fprintf(stdout, "override-unmatched  %s:%d  %s %s (no such break in this diff)\n", *overridesPath, o.line, o.surface, o.symbol)
		}
	}
	_, _ = fmt.Fprintf(stdout, "\n%d breaking change(s), %d waived, %d unwaived\n", len(breaks), len(breaks)-len(unwaived), len(unwaived))
	if len(unwaived) == 0 {
		return 0
	}
	_, _ = fmt.Fprintf(stdout, "\nIf the break is intended, add this line to %s (fill the issue and reason:\nwhy it is intended and how consumers such as substrate migrate), and justify it in the PR:\n\n", *overridesPath)
	exp := suggestExpiry()
	for _, b := range unwaived {
		_, _ = fmt.Fprintf(stdout, "%s | %s | %s | <issue, e.g. elps#123 substrate#456> | <reason and consumer migration>\n", b.surface, b.symbol, exp)
	}
	return 1
}
