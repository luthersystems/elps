#!/usr/bin/env bash
# Copyright © 2021 Luther Systems, Ltd. All right reserved.

# This script runs source code checking tools it can be called via a make
# target or can be run automatically during CI testing.

set -exuo pipefail

go vet ./...

GOSEC_EXCLUSIONS="${GOSEC_EXCLUSIONS:-}"
# gosec can't handle vanilla fabric chaincode due to how fabric injects the
# shim dynamically.
gosec -exclude="$GOSEC_EXCLUSIONS" -exclude-dir _examples/wasm/app ./...

staticcheck ./...
