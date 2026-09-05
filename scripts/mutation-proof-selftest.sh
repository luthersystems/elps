#!/usr/bin/env bash
# mutation-proof-selftest.sh -- the control on the control.
#
# mutation-proof.sh makes three guarantees, and a harness that cannot fail
# correctly proves nothing:
#
#   1. a patch that no longer applies FAILS LOUDLY and is never skipped
#   2. a mutation that does not COMPILE is not counted as a catch
#   3. a CLEAN TREE THAT IS ALREADY RED is refused, so unrelated breakage
#      cannot report every mutation as caught
#
# Those three were verified by hand once. Verifying them once is the exact
# gap mutation-proof.sh itself exists to close -- a proof that held one
# afternoon and guards nothing afterwards -- so they are verified here
# instead, on every run.
#
# Each case DELIBERATELY BREAKS something and requires mutation-proof.sh to
# exit non-zero with the right message. A case that passes is a bug.
set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2
ROOT=$(pwd)
PROOF="$ROOT/scripts/mutation-proof.sh"
fail=0

cleanup() { git -C "$ROOT" checkout -- lisp/ elpstest/ 2>/dev/null; rm -f "$ROOT/scripts/mutations/zz-selftest-noncompiling.patch"; }
trap cleanup EXIT

[ -z "$(git -C "$ROOT" status --porcelain -- lisp/ elpstest/)" ] ||
  { echo "selftest: lisp/ or elpstest/ is dirty; commit or stash first." >&2; exit 2; }

check() { # name, expected-substring, log
  if grep -qF -- "$2" "$3"; then echo "  ok   $1"; else
    echo "  FAIL $1 -- expected to see: $2" >&2; sed 's/^/       /' "$3" >&2; fail=1
  fi
}

echo "== 1. a patch that no longer applies must fail loudly, never skip =="
victim="$ROOT/scripts/mutations/576-fork-map-memo.patch"
cp "$victim" /tmp/mpst-victim.keep
sed -i 's/f\.maps\[md\]/f.RENAMED[md]/' "$victim"
bash "$PROOF" 576-fork-map-memo >/tmp/mpst1.log 2>&1; rc=$?
cp /tmp/mpst-victim.keep "$victim"
[ $rc -ne 0 ] || { echo "  FAIL exit 0 on a non-applying patch" >&2; fail=1; }
check "loud, and names the patch" "PATCH DOES NOT APPLY" /tmp/mpst1.log

echo "== 2. a mutation that does not compile is not a catch =="
python3 - <<'PY'
p = "lisp/fork.go"
s = open(p).read()
anchor = "func (f *forker) mapData(md *MapData) *MapData {"
assert anchor in s, "selftest anchor moved"
open(p, "w").write(s.replace(anchor, anchor + "\n\tthis is not go\n", 1))
PY
git -C "$ROOT" diff -- lisp/ > "$ROOT/scripts/mutations/zz-selftest-noncompiling.patch"
git -C "$ROOT" checkout -- lisp/
MP_EXTRA_ROW='zz-selftest-noncompiling;a fresh fork is indistinguishable from its template;' \
  bash "$PROOF" zz-selftest-noncompiling >/tmp/mpst2.log 2>&1; rc=$?
[ $rc -ne 0 ] || { echo "  FAIL exit 0 on a non-compiling mutation" >&2; fail=1; }
check "reported as broken, not as caught" "DOES NOT COMPILE" /tmp/mpst2.log
grep -qE '^[A-Za-z0-9._-]+ +caught$' /tmp/mpst2.log && { echo "  FAIL a non-compiling mutation was counted as caught" >&2; fail=1; }
rm -f "$ROOT/scripts/mutations/zz-selftest-noncompiling.patch"

echo "== 3. an already-red clean tree is refused =="
python3 - <<'PY'
p = "elpstest/aliasguard_internal_test.go"
s = open(p).read()
anchor = "func TestQuoteKeyDoesNotDoubleQuote(t *testing.T) {\n\tt.Parallel()"
assert anchor in s, "selftest anchor moved"
open(p, "w").write(s.replace(anchor, anchor + '\n\tt.Error("mutation-proof selftest: simulated unrelated failure")', 1))
PY
bash "$PROOF" >/tmp/mpst3.log 2>&1; rc=$?
git -C "$ROOT" checkout -- elpstest/
[ $rc -ne 0 ] || { echo "  FAIL exit 0 with an already-red suite" >&2; fail=1; }
check "refused before any mutation ran" "the CLEAN tree is already red" /tmp/mpst3.log
# The refusal message itself contains the word "caught", so match the
# RESULT-LINE shape rather than the bare word.
grep -qE '^[A-Za-z0-9._-]+ +caught$' /tmp/mpst3.log && { echo "  FAIL mutations ran against a red tree" >&2; fail=1; }

echo
[ $fail = 0 ] || { echo "mutation-proof-selftest: FAILED" >&2; exit 1; }
echo "mutation-proof-selftest: all three guarantees hold."
