#!/usr/bin/env python3
"""Every hand copy of the Go version must equal go.mod's toolchain line.

CI sets up Go with `go-version-file: go.mod`, which installs go.mod's
`toolchain` line exactly. A few places still copy that version by hand:
benchmark.yml's GO_VERSION (it builds two refs in one job, so it cannot read
one go.mod), the session-start hook's GO_TOOLCHAIN and cloud-web-setup.sh's
GO_VERSION. A bump that misses one of them leaves it on the old release with
nothing failing, so this test fails instead.
"""

import pathlib
import re
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent

# (file, regex whose group 1 is the version)
COPIES = (
    (".github/workflows/benchmark.yml", r'^\s*GO_VERSION:\s*"([^"]+)"'),
    (".claude/hooks/session-start.sh", r'^GO_TOOLCHAIN="go([^"]+)"'),
    ("scripts/cloud-web-setup.sh", r"^GO_VERSION=(\S+)"),
)


def gomod_toolchain(source):
    found = re.findall(r"^toolchain go([0-9][0-9.]*)", source, re.M)
    if len(found) != 1:
        raise ValueError("go.mod must have exactly one toolchain line")
    return found[0]


def copy_versions(source, pattern):
    found = re.findall(pattern, source, re.M)
    if not found:
        raise ValueError(f"no version matched {pattern!r}")
    return found


def check(gomod, copies):
    want = gomod_toolchain(gomod)
    for name, (source, pattern) in copies.items():
        for got in copy_versions(source, pattern):
            if got != want:
                raise ValueError(f"{name} pins {got} but go.mod's toolchain is go{want}")


class GoPinSync(unittest.TestCase):
    def setUp(self):
        self.gomod = (ROOT / "go.mod").read_text()
        self.copies = {
            name: ((ROOT / name).read_text(), pattern) for name, pattern in COPIES
        }

    def test_real_tree(self):
        check(self.gomod, self.copies)

    def test_negative_controls(self):
        want = gomod_toolchain(self.gomod)
        with self.subTest(fault="go.mod bumped alone"):
            with self.assertRaises(ValueError):
                check(self.gomod.replace(f"toolchain go{want}", "toolchain go1.99.0"), self.copies)
        with self.subTest(fault="toolchain line removed"):
            with self.assertRaises(ValueError):
                check(self.gomod.replace(f"toolchain go{want}\n", ""), self.copies)
        for name, (source, pattern) in self.copies.items():
            mutated = source.replace(want, "1.99.0")
            with self.subTest(fault=f"{name} drifted"):
                self.assertNotEqual(source, mutated, "negative control did not change the file")
                with self.assertRaises(ValueError):
                    check(self.gomod, {**self.copies, name: (mutated, pattern)})


if __name__ == "__main__":
    unittest.main()
