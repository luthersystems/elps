#!/usr/bin/env python3
"""Pin scanner installation to the audited Go-compatible release (#634).

CI still performs the real install and queries the live vulnerability database.
This contract detects floating/removed pins, not future scanner vulnerabilities.

The scan's Go comes from go.mod's `toolchain` line (setup-go
`go-version-file: go.mod`). The pinned scanner needs Go >= MIN_GO, so the test
fails when that line drops below it; it does not restate the version itself.
"""

import pathlib
import re
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
INSTALL = "go install golang.org/x/vuln/cmd/govulncheck@v1.8.0"
MIN_GO = (1, 26)  # golang.org/x/vuln v1.8.0 requires Go 1.26
SETUP = "go-version-file: go.mod"


def check_workflow(source):
    active = "\n".join(line.split("#", 1)[0] for line in source.splitlines())
    installs = re.findall(r"go install golang\.org/x/vuln/cmd/govulncheck@\S+", active)
    if installs != [INSTALL]:
        raise ValueError("expected exactly one Go-1.26-compatible scanner install")
    if re.findall(r"go-version(?:-file)?:\s*\S+", active) != [SETUP]:
        raise ValueError("the scan must set up Go from go.mod's toolchain line")


def check_gomod(source):
    found = re.findall(r"^toolchain go(\d+)\.(\d+)", source, re.M)
    if len(found) != 1:
        raise ValueError("go.mod must have exactly one toolchain line")
    if tuple(int(x) for x in found[0]) < MIN_GO:
        raise ValueError("the pinned scanner needs a newer Go than go.mod's toolchain line")


class ScannerToolchainContract(unittest.TestCase):
    def test_real_workflows_and_negative_controls(self):
        for name in ("govulncheck.yml", "govulncheck-scheduled.yml"):
            source = (ROOT / ".github" / "workflows" / name).read_text()
            with self.subTest(workflow=name):
                check_workflow(source)
            for label, mutated in (
                ("floating", source.replace(INSTALL, INSTALL.replace("v1.8.0", "latest"))),
                ("superseded", source.replace(INSTALL, INSTALL.replace("v1.8.0", "v1.7.0"))),
                ("removed", source.replace(INSTALL, "")),
                ("commented", source.replace(INSTALL, "# " + INSTALL)),
                ("duplicate", source + "\n" + INSTALL),
                ("pinned", source.replace(SETUP, 'go-version: "1.27.0"')),
                ("stable", source.replace(SETUP, 'go-version: "stable"')),
            ):
                with self.subTest(workflow=name, fault=label):
                    self.assertNotEqual(source, mutated, "negative control did not change workflow")
                    with self.assertRaises(ValueError):
                        check_workflow(mutated)

    def test_gomod_toolchain_and_negative_controls(self):
        source = (ROOT / "go.mod").read_text()
        check_gomod(source)
        line = re.search(r"^toolchain go\S+$", source, re.M).group(0)
        for label, mutated in (
            ("below minimum", source.replace(line, "toolchain go1.25.13")),
            ("removed", source.replace(line + "\n", "")),
        ):
            with self.subTest(fault=label):
                self.assertNotEqual(source, mutated, "negative control did not change go.mod")
                with self.assertRaises(ValueError):
                    check_gomod(mutated)


if __name__ == "__main__":
    unittest.main()
