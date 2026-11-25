#!/usr/bin/env bash
# Prints elps's own section of the release notes: "## Breaking changes", one
# bullet per API break override (scripts/api-breaks.txt) added since the
# previous release tag. Prints nothing when there is none.
#
# release-tag.yml passes it to the shared release workflow as
# notes_extra_command. The shared workflow writes the list of changes, then
# appends this output. It sets LAST_TAG (empty for a first release) and TAG.
#
# Every waived API break must be listed in the release notes. Never omit one.
#
# Usage: LAST_TAG=v1.2.3 bash scripts/release-notes-extra.sh
set -euo pipefail

last_tag="${LAST_TAG:-}"

breaks=$(bash "$(dirname "${BASH_SOURCE[0]}")/api-breaks-since.sh" "${last_tag:-none}")
if [[ "$breaks" == "(none)" ]]; then
  exit 0
fi

echo "## Breaking changes"
echo
echo "API break overrides added since ${last_tag:-the first release} (surface | symbol | expires | issue | reason):"
echo
while IFS= read -r line; do
  echo "- $line"
done <<< "$breaks"
