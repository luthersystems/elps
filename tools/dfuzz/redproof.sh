#!/usr/bin/env bash
# Copyright © 2026 The ELPS authors
#
# Red-proof: inject a copy-on-write leak into the tree under test and confirm
# dfuzz reports it as a FINDING rather than excusing it via the allowlist.
#
#   ./redproof.sh <workdir>
#
# A clean differential run proves nothing on its own -- an oracle that cannot
# fail is indistinguishable from an oracle that is not looking.  This injects
# the exact defect class the harness exists to catch: `reverse` reverses its
# INPUT in place as well as returning a reversed copy.  The write is a raw
# slice write, so it goes around the copy-on-write guards the same way a real
# regression would, and it is deliberately a builtin no allowlist rule names.
#
# Expected: findings on the seed corpus (the two `reverse` shapes) and on a
# few percent of generated programs, with the allowlist excusing none of them.

set -euo pipefail

W=${1:-/tmp/dfuzz-red}
HERE=$(cd "$(dirname "$0")" && pwd)

"$HERE/setup.sh" "${LEFT_REF:-origin/main}" "${RIGHT_REF:-HEAD}" "$W" >/dev/null

python3 - "$W" <<'PY'
import sys
p = sys.argv[1] + "/right/lisp/builtins.go"
s = open(p).read()
old = """	for i, v := range seqCells(list) {
		cells[len(cells)-1-i] = v
	}
	return v
}

func builtinSlice"""
if old not in s:
    raise SystemExit("builtinReverse does not have the expected shape; update redproof.sh")
new = """	for i, v := range seqCells(list) {
		cells[len(cells)-1-i] = v
	}
	// INJECTED REGRESSION (red-proof only): reverse the INPUT in place too.
	// A raw slice write, so it goes around the copy-on-write guards exactly
	// as a real regression would.
	sc := seqCells(list)
	for i, j := 0, len(sc)-1; i < j; i, j = i+1, j-1 {
		sc[i], sc[j] = sc[j], sc[i]
	}
	return v
}

func builtinSlice"""
open(p, "w").write(s.replace(old, new))
print("injected the reverse-in-place regression")
PY

cd "$W/harness" && go build -o "$W/dfuzz" .

echo "=== seed corpus (expect FINDINGs on the reverse shapes)"
"$W/dfuzz" -seeds || true
echo "=== 2000 generated programs"
"$W/dfuzz" -n 2000 -workers 2 2>&1 | grep -E "^(program pairs|diverged|  intended|    |  findings)" || true
