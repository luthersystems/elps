#!/usr/bin/env bash
# mutation-proof.sh -- revert each real historical fix and prove the guard
# catches it.
#
# WHY THIS EXISTS, when there are already ten broken reference walkers in
# elpstest/aliasguard_broken_test.go: those model the bugs with hand-written
# imitations. This reverts the ACTUAL FIXES in production code. The PR that
# built the guard did that too -- by hand, once, in a scratch worktree, never
# committed -- which proved the guard worked that afternoon and guards nothing
# afterwards. That is the same one-time-proof gap the committed negative
# controls were added to close, and it stayed open for the real defects.
#
# WHY PATCHES AND NOT `git revert`. Measured: 5 of the 7 historical fix commits
# no longer reverse-apply (d1fdc69, 723403e, 742598b, 0d0ecb7, 2bedc6f all
# conflict; only 601c235 and f824d81 apply). Later commits touched the same
# code. So the mutations are hand-maintained patches against CURRENT code, and
# rule 1 below is what keeps them honest as the code keeps moving.
#
# THREE RULES, each of which this script would be worthless without:
#
#  1. A PATCH THAT NO LONGER APPLIES FAILS LOUDLY, NEVER SKIPS. Code moves. A
#     silently-skipped mutation is a green run that proved nothing.
#  2. A BUILD FAILURE IS NOT A CATCH. This trap has been hit for real on this
#     branch: a weakened tree that did not compile looked like a red control.
#     `go build ./...` must succeed after applying a mutation, BEFORE any test
#     runs. A mutation that does not compile is a broken mutation.
#  3. THE SPECIFIC PROPERTY IS ASSERTED, not "something failed". A mutation
#     that reddens the suite for an unrelated reason is not proof.
#
# And a precondition: the CLEAN TREE MUST BE GREEN first, so a suite that is
# red for unrelated reasons cannot report every mutation as caught.
#
# MUST_NOT is as load-bearing as MUST. Recording what a bug does NOT trip is
# how the #576 attribution error was found: the guard's own doc claimed it
# failed property 1, which is structurally impossible for a de-aliasing defect.
#
# Usage:  scripts/mutation-proof.sh [name ...]     (default: all)
set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 2
ROOT=$(pwd)
DIR="$ROOT/scripts/mutations"
PKG=./elpstest/

# name ; must-trip (|-separated) ; must-NOT-trip (|-separated, may be empty)
#
# The field separators are DISTINCT on purpose. They were both '|' once, so a
# '||' in a row produced an empty outer field and every must-trip entry after
# it was read as a must-NOT-trip. The script failed loudly on that rather than
# passing, which is the behaviour to keep, but the row format was the bug.
#
# Every row was MEASURED, not assumed. Notes on the surprising ones:
#  - 576 does NOT trip property 1 or property 5. It cannot: de-aliasing makes a
#    fork copy MORE, so a fork write can never reach the template, and both of
#    those properties can only redden on OVER-sharing.
#  - 397 trips property 5 as well as 1/2/3, which the original hand-run
#    revert-proof table did not record.
#  - 585 reddens copy and Detach but NOT Fork, which is the whole point of #585.
#  - 579 names a TEST rather than a property because WHICH tests catch it varies
#    run to run. Measured over 10 runs it was caught 10/10, but by a varying
#    set: TestAliasGuardOverEveryWalker 10/10, TestAliasGuardSelfReferentialMap
#    9/10, TestForkFingerprintsIdenticallyToItsTemplate 8/10. An expectation
#    naming any of the less-than-10/10 signals would be a FLAKY GATE. Pin the
#    stable signal, and never widen a row without measuring its stability.
MANIFEST=$(cat <<'EOF'
576-fork-map-memo;a fresh fork is indistinguishable from its template|a fork taken after other forks were mutated is pristine;the template is unchanged by a transaction on a fork|a transaction on the template is invisible to every existing fork
579-stock-map-memo-seeded-late;TEST:TestAliasGuardOverEveryWalker;
585-detach-memos;TEST:TestAliasGuardOverEveryWalker|TEST:TestAliasGuardSelfReferentialMap;
397-fork-shares-funnames;the template is unchanged by a transaction on a fork|a transaction on one fork is invisible to every other fork|a transaction on the template is invisible to every existing fork;
440-fork-carries-loc;a fork starts with an empty evaluator location register;
578-f1-live-defining-loc;TEST:TestLocationChannelHasNoBleed;
582-macro-stamp-in-place;expansion mutates nothing reachable outside its own output;
template-share;a transaction on the template is invisible to every existing fork|a fresh fork is indistinguishable from its template;
EOF
)

# MP_EXTRA_ROW appends one manifest row at run time. It exists solely for
# scripts/mutation-proof-selftest.sh, which has to feed this script a
# deliberately non-compiling mutation to prove that such a mutation is
# reported as BROKEN rather than counted as a catch. Unset in normal use.
[ -z "${MP_EXTRA_ROW:-}" ] || MANIFEST="$MANIFEST
$MP_EXTRA_ROW"

die() { echo "mutation-proof: $*" >&2; exit 1; }

restore() { git -C "$ROOT" checkout -- lisp/ 2>/dev/null; }
trap restore EXIT

[ -z "$(git -C "$ROOT" status --porcelain -- lisp/)" ] ||
  die "lisp/ has uncommitted changes; this script rewrites it. Commit or stash first."

echo "== precondition: the clean tree must be green =="
if ! go test "$PKG" -count=1 >/tmp/mp-clean.log 2>&1; then
  echo "--- clean-tree output ---" >&2; tail -30 /tmp/mp-clean.log >&2
  die "the CLEAN tree is already red. Every mutation would look 'caught'. Fix the suite first."
fi
echo "   clean tree green"

want=("$@")
fail=0
printf '\n%-34s %s\n' "MUTATION" "RESULT"
while IFS=';' read -r name must mustnot; do
  [ -n "$name" ] || continue
  if [ ${#want[@]} -gt 0 ]; then
    match=0; for w in "${want[@]}"; do [ "$w" = "$name" ] && match=1; done
    [ $match = 1 ] || continue
  fi
  patch="$DIR/$name.patch"

  # RULE 1: a missing or non-applying patch is a hard failure.
  [ -f "$patch" ] || { printf '%-34s NO PATCH FILE (%s)\n' "$name" "$patch"; fail=1; continue; }
  if ! git -C "$ROOT" apply "$patch" 2>/tmp/mp-apply.log; then
    printf '%-34s PATCH DOES NOT APPLY -- the code moved.\n' "$name"
    echo "    Regenerate $patch against current lisp/. A skipped mutation proves nothing," >&2
    echo "    which is why this is an error and not a warning. git apply said:" >&2
    sed 's/^/      /' /tmp/mp-apply.log >&2
    fail=1; continue
  fi

  # RULE 2: a mutation that does not compile is broken, not caught.
  if ! go build ./... >/tmp/mp-build.log 2>&1; then
    printf '%-34s DOES NOT COMPILE -- broken mutation, NOT a catch.\n' "$name"
    sed 's/^/      /' /tmp/mp-build.log >&2
    restore; fail=1; continue
  fi

  out=$(go test "$PKG" -count=1 2>&1)
  restore

  # RULE 3: assert the SPECIFIC property, not that something failed.
  missing=()
  IFS='|' read -ra musts <<<"$must"
  for m in "${musts[@]}"; do
    [ -n "$m" ] || continue
    needle=${m#TEST:}
    case "$m" in
      TEST:*) grep -q -- "--- FAIL: $needle" <<<"$out" || missing+=("$m") ;;
      *)      grep -qF -- "$needle" <<<"$out" || missing+=("$m") ;;
    esac
  done
  wrong=()
  IFS='|' read -ra notes <<<"${mustnot:-}"
  for m in "${notes[@]}"; do
    [ -n "$m" ] || continue
    grep -qF -- "$m" <<<"$out" && wrong+=("$m")
  done

  if [ ${#missing[@]} -eq 0 ] && [ ${#wrong[@]} -eq 0 ]; then
    printf '%-34s caught\n' "$name"
  else
    printf '%-34s NOT PROVEN\n' "$name"
    for m in "${missing[@]:-}"; do [ -n "$m" ] && echo "      expected to trip but did not: $m" >&2; done
    for m in "${wrong[@]:-}";   do [ -n "$m" ] && echo "      tripped but must NOT: $m" >&2; done
    fail=1
  fi
done <<<"$MANIFEST"

echo
[ $fail = 0 ] || die "at least one mutation was not proven -- see above."
echo "mutation-proof: every mutation caught by its named property."
