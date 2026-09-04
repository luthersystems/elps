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
# THE COST OF HAND-MAINTAINING THEM, learned the hard way: a hand-written
# patch can revert something OTHER than the fix it is named for, and nothing
# mechanical catches that. The first 579 row moved a memo seed in forker's
# stock sorted-map path -- a line whose own production comment cites #576 --
# so it reverted a second #576-class protection under the wrong issue number,
# while this header claimed every mutation reverts "the ACTUAL FIXES". Where
# a real fix commit still reverse-applies, DERIVE THE PATCH FROM IT rather
# than hand-writing one: 579-libschema-validator-credential.patch is
# `git show 6ef3da5` reversed, and its provenance is checkable.
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
# EVERY NEEDLE HERE IS A PROPERTY STRING OR A UNIQUE TEST, MEASURED FOR BOTH
# UNIQUENESS AND STABILITY. An earlier revision pinned TEST: names shared by
# five of the eight mutations, which asserted "a test failed" rather than the
# property -- the exact thing rule 3 forbids -- and pinned one signal that was
# only ~84% stable, making this required gate flaky about one run in six. A
# flaky required gate is worse than no gate: it trains maintainers to re-run
# it, then to delete it, which destroys the anti-regression value this exists
# for. Do not add a needle without measuring it across ALL mutations.
#
# Notes on the rows that are not obvious:
#  - 576 does NOT trip property 1 or property 5, asserted here as MUST-NOT. It
#    cannot: de-aliasing makes a fork copy MORE, so a fork write can never
#    reach the template, and both of those can only redden on OVER-sharing.
#  - 585's needle carries the WALKER PREFIX ("Detach: ..."). Without it the
#    string is shared with 576 and the row distinguishes nothing; with it the
#    row asserts #585's actual signature -- copy and Detach redden, Fork does
#    not -- and the must-not on the fresh-fork property pins the "not Fork"
#    half.
#  - template-share pins ONLY property 5, which is the direction it models.
#    It also emits the fresh-fork property, and that needle measured 40/40 in
#    ISOLATION -- yet failed once in 35 end-to-end runs. Isolated measurement
#    therefore does NOT certify a needle: witness output varies with test
#    ordering and parallelism under load, and only the end-to-end run
#    reproduces that. Measure candidate needles with the full script, many
#    times, not by driving one mutation in a loop.
#  - 579 is the libschema validator credential (6ef3da5), NOT a fork memo. An
#    earlier revision of this file shipped a patch that moved a memo seed in
#    forker's stock-map path and labelled it #579; that line's own production
#    comment cites #576, so the mutation reverted a second #576-class
#    protection under the wrong issue number, and it was the flaky row. The
#    real #579 revert is deterministic.
MANIFEST=$(cat <<'EOF'
576-fork-map-memo;a fresh fork is indistinguishable from its template;the template is unchanged by a transaction on a fork|a transaction on the template is invisible to every existing fork
579-libschema-validator-credential;TEST:TestForkCheck_SchemaValidatorCredential;
585-detach-memos;Detach: the copy has the same mutable payloads as the source;a fresh fork is indistinguishable from its template
397-fork-shares-funnames;the template is unchanged by a transaction on a fork|a transaction on one fork is invisible to every other fork;
440-fork-carries-loc;a fork starts with an empty evaluator location register;
578-f1-live-defining-loc;a budget error at a function-body entry reports the definition site;
582-macro-stamp-in-place;expansion mutates nothing reachable outside its own output;
template-share;a transaction on the template is invisible to every existing fork;
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
