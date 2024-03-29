#!/usr/bin/env bash
# Copyright © 2021 Luther Systems, Ltd. All right reserved.

# This script runs source code checking tools it can be called via a make
# target or can be run automatically during CI testing.

set -exuo pipefail

# work around for golangci-lint -buildvcs=false
git config --global --add safe.directory $PWD
golangci-lint version
golangci-lint run -v
