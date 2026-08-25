package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"strconv"
	"time"
)

// envDefaultFloat reads a policy number from the environment, falling back to
// def. Both repositories already declare these names as workflow-level `env:`
// entries next to the prose explaining how each number was measured, so
// honouring them is what makes this a migration rather than a policy change.
func envDefaultFloat(name string, def float64) (float64, string, error) {
	raw, ok := os.LookupEnv(name)
	if !ok || raw == "" {
		return def, trimFloat(def), nil
	}
	v, err := strconv.ParseFloat(raw, 64)
	if err != nil {
		return 0, "", fmt.Errorf("%s=%q is not a number", name, raw)
	}
	// The raw string, not the parsed float, is what the report prints: a
	// workflow that says 15 must not have its gate reported as 15.000000.
	return v, raw, nil
}

func trimFloat(v float64) string {
	return strconv.FormatFloat(v, 'g', -1, 64)
}

var isoDate = regexp.MustCompile(`^[0-9]{4}-[0-9]{2}-[0-9]{2}$`)

const usage = `usage: benchgate [flags] <benchstat-output-file>
       benchgate [flags] -base <go-test-output> -head <go-test-output>

Adjudicate a benchmark comparison: exit 0 clean, 1 regression, 2 could not be
interpreted.

The first form reads a benchstat table (what CI already produces for the PR
comment). The second reads raw ` + "`go test -bench`" + ` output for each arm and
computes the comparison itself with golang.org/x/perf/benchfmt and benchmath --
no benchstat binary required.

flags (each falls back to the matching BENCH_* environment variable, which is
how both consuming repositories declare their policy):
  -threshold N        timing metrics: sec/op, B/s, unrecognised
                      (env BENCH_REGRESSION_THRESHOLD_PCT, default 15)
  -alloc-threshold N  allocation metrics: B/op, allocs/op
                      (env BENCH_ALLOC_THRESHOLD_PCT, default 5)
  -alpha N            significance level (env BENCH_ALPHA, default 0.05)
  -waivers PATH       reviewed waiver list (env BENCH_WAIVERS; empty = none).
                      Named explicitly, so a path that is not there is an
                      error rather than "no waivers configured".
  -waivers-default P  the repository's shipped waiver list, used only when
                      neither -waivers nor BENCH_WAIVERS is set. Its ABSENCE
                      is not an error -- the gate still works, it just has no
                      waivers, which is the strict direction.
  -today YYYY-MM-DD   the date waiver expiry is judged against
                      (env BENCH_WAIVER_TODAY, default today UTC)
  -base FILE          base arm, raw go test -bench output
  -head FILE          head arm, raw go test -bench output
`

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

// pf, pln and pr wrap the fmt.Fprint* family for the stdout/stderr this tool
// writes its report to. Their errors are deliberately discarded: a benchmark
// gate that cannot write its own verdict to stdout has nowhere to fall back to,
// and errcheck's own default exclusion already ignores exactly these calls when
// the target is the os.Stdout / os.Stderr *literal*. benchgate injects its
// writers so run() is testable end to end, which the literal-matching exclusion
// cannot see -- so the discard is made explicit here rather than repeated at
// every call site.
func pf(w io.Writer, format string, a ...any) { _, _ = fmt.Fprintf(w, format, a...) }
func pln(w io.Writer, a ...any)               { _, _ = fmt.Fprintln(w, a...) }
func pr(w io.Writer, a ...any)                { _, _ = fmt.Fprint(w, a...) }

// run is main() with its I/O injected, so the whole CLI -- flag parsing, the
// env fallbacks, the exit codes -- is exercised by the tests rather than only
// the pieces underneath it.
func run(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("benchgate", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() { pr(stderr, usage) }

	thrDef, thrStr, err1 := envDefaultFloat("BENCH_REGRESSION_THRESHOLD_PCT", 15)
	allocDef, allocStr, err2 := envDefaultFloat("BENCH_ALLOC_THRESHOLD_PCT", 5)
	alphaDef, _, err3 := envDefaultFloat("BENCH_ALPHA", 0.05)
	for _, err := range []error{err1, err2, err3} {
		if err != nil {
			pf(stderr, "benchgate: %v.\n", err)
			return 2
		}
	}

	threshold := fs.Float64("threshold", thrDef, "timing-metric regression gate, percent")
	allocThreshold := fs.Float64("alloc-threshold", allocDef, "allocation-metric regression gate, percent")
	alpha := fs.Float64("alpha", alphaDef, "significance level")
	base := fs.String("base", "", "base arm: raw `go test -bench` output")
	head := fs.String("head", "", "head arm: raw `go test -bench` output")

	// The waiver path has THREE states, not two, and each behaves differently
	// when the file is missing:
	//
	//   explicit path   (-waivers / BENCH_WAIVERS)  -> must exist; exit 2
	//   explicit empty  (-waivers= / BENCH_WAIVERS=) -> no waivers at all,
	//                    which can only make the gate stricter, so it is not
	//                    a bypass
	//   neither set     -> fall back to -waivers-default, whose ABSENCE is
	//                    fine: the gate works, it just has no waivers
	//
	// A plain string flag collapses the first two, hence the explicit lookup.
	waiverPath, waiverExplicit := os.LookupEnv("BENCH_WAIVERS")
	fs.Func("waivers", "reviewed waiver list; pass an empty value for none", func(v string) error {
		waiverPath, waiverExplicit = v, true
		return nil
	})
	waiverDefault := fs.String("waivers-default", "", "the repository's shipped waiver list, used only when -waivers/BENCH_WAIVERS is unset; its absence is not an error")

	today := os.Getenv("BENCH_WAIVER_TODAY")
	if today == "" {
		today = time.Now().UTC().Format("2006-01-02")
	}
	fs.StringVar(&today, "today", today, "date waiver expiry is judged against, YYYY-MM-DD")

	if err := fs.Parse(args); err != nil {
		return 2
	}

	if !isoDate.MatchString(today) {
		pf(stderr, "benchgate: -today/BENCH_WAIVER_TODAY=%q is not a YYYY-MM-DD date.\n", today)
		return 2
	}

	// Reconstruct the printed threshold strings when a flag overrode the env.
	fs.Visit(func(f *flag.Flag) {
		switch f.Name {
		case "threshold":
			thrStr = f.Value.String()
		case "alloc-threshold":
			allocStr = f.Value.String()
		}
	})

	rawMode := *base != "" || *head != ""
	rest := fs.Args()
	switch {
	case rawMode && (*base == "" || *head == ""):
		pln(stderr, "benchgate: -base and -head must be given together.")
		pr(stderr, usage)
		return 2
	case rawMode && len(rest) > 0:
		pln(stderr, "benchgate: -base/-head takes no positional table argument.")
		pr(stderr, usage)
		return 2
	case !rawMode && len(rest) != 1:
		pr(stderr, usage)
		return 2
	}

	if !waiverExplicit {
		waiverPath = *waiverDefault
	}
	ws, rc := loadWaivers(waiverPath, waiverExplicit, today, stdout, stderr)
	if rc != 0 {
		return rc
	}

	p := &policy{
		alpha:             *alpha,
		threshold:         *threshold,
		allocThreshold:    *allocThreshold,
		thresholdStr:      thrStr,
		allocThresholdStr: allocStr,
		waivers:           ws,
		waiverSource:      ws.source,
	}

	var c *comparison
	var inputDesc string
	if rawMode {
		var unpaired int
		var err error
		c, unpaired, err = compareArms(*base, *head, *alpha, stderr)
		if err != nil {
			pf(stderr, "benchgate: %v\n", err)
			return 2
		}
		inputDesc = fmt.Sprintf("%s vs %s", *base, *head)
		if unpaired > 0 {
			// Not folded into the verdict: unpaired cells are an arm
			// comparability problem, which scripts/bench-arms-check.sh is the
			// designated guard for. Printed so they cannot vanish.
			pf(stdout, "benchgate: %d metric cell(s) appear in only one arm and were not adjudicated; run scripts/bench-arms-check.sh if that is unexpected.\n", unpaired)
		}
	} else {
		path := rest[0]
		st, err := os.Stat(path)
		if err != nil {
			pf(stderr, "benchgate: no such file: %s\n", path)
			return 2
		}
		if st.Size() == 0 {
			pf(stderr, "benchgate: %s is empty -- benchstat produced no output\n", path)
			return 2
		}
		b, err := os.ReadFile(path) //#nosec G304 -- benchgate is a CLI given the benchstat table path to read
		if err != nil {
			pf(stderr, "benchgate: %v\n", err)
			return 2
		}
		c = parseTable(string(b))
		inputDesc = path
	}

	v := adjudicate(c, p)
	return v.report(stdout, stderr, p, inputDesc)
}

// loadWaivers resolves the three-state waiver path. An EXPLICIT path pointing
// at nothing is an error -- you asked for a specific file and it is not there.
// The default file being absent is not: the gate still works, it just has no
// waivers, which is the strict direction.
func loadWaivers(path string, explicit bool, today string, stdout, stderr io.Writer) (*waiverSet, int) {
	if path == "" {
		// No default is baked into the binary: the two consuming repositories
		// keep their waiver lists in different places, and a tool that guessed
		// one would silently adjudicate with the wrong list. Callers pass
		// -waivers-default; nothing means no waivers.
		return parseWaivers("", "", today), 0
	}
	b, err := os.ReadFile(path) //#nosec G304 -- benchgate is a CLI given the waiver-list path to read
	if err != nil {
		if !explicit {
			pf(stdout, "benchgate: no waiver file at %s; adjudicating with no waivers.\n", path)
			return parseWaivers(path, "", today), 0
		}
		pf(stderr, "benchgate: -waivers/BENCH_WAIVERS points at a file that does not exist: %s\n", path)
		return nil, 2
	}
	return parseWaivers(path, string(b), today), 0
}
