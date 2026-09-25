# VS Code Marketplace diagnostics and recovery

The Go release and the Marketplace extension are separate publications. A
published Go module does not establish that every VSIX target is available.

## Diagnose without publishing

The existing `Publish VS Code Extension` workflow has a **read-only manual
mode**. Tag pushes still build and publish the original five packages. Manual
runs cannot build or publish, and do not receive `VSCE_PAT`.

```sh
gh workflow run vscode-publish.yml --repo luthersystems/elps --ref <reviewed-branch>
```

The diagnostic job uses Ubuntu 24.04 ARM and Node 20, matching publication.
It reports only public addresses, timings, HTTP status codes, and a validated
version/platform inventory for `LutherSystems.elps-lang`:

- unauthenticated `OPTIONS /_apis/gallery`, including DNS, connect, TLS and
  first-byte timing;
- the public, read-only extension query (POST is the API's query method);
- the locked `azure-devops-node-api` client's `getExtension` call used by `vsce`
  before upload, **without** an authentication handler.

Raw requests have a 15-second absolute deadline and a 1 MiB response limit. The
SDK subprocess has an independent 20-second deadline. Response headers, bodies,
exception messages, cookies, and environment variables are never logged. The
SDK subprocess receives an empty environment; dependency lifecycle scripts are
disabled. Its authenticated behavior is deliberately not tested.

**A green diagnostic job means the diagnostic ran, not that publishing works.**
Read its JSON report. `401` from unauthenticated discovery proves an HTTP
response arrived, not that a PAT is invalid. A successful public query establishes
what is currently indexed, not whether an unindexed upload has been accepted.
Compare raw transport with the SDK result before attributing a failure to the
client. An SDK-only failure warrants examining
the locked client's request behavior; neither result justifies rotating secrets.

Local checks (Node and Python with PyYAML, also used by the existing CI guards):

```sh
node --test scripts/marketplace-diagnostics.test.cjs
python3 scripts/marketplace-workflow-test.py
```

## Recovery requires an explicitly approved publishing operation

1. Record the diagnostic run and the desired release's five targets: universal,
   linux-x64, linux-arm64, darwin-x64, darwin-arm64. Confirm missing targets in the
   publisher's management view too: the public index may lag an accepted upload.
2. Resolve the diagnosed cause. Do not speculate about PAT expiry from a network
   timeout, or switch runner architectures merely because the failing host is ARM.
3. Publish **only missing targets**, using the immutable release source and
   artifacts. A failed-job rerun is safe only after checking that none of those
   jobs already uploaded its target. Do not move a release tag or rerun all jobs
   blindly. Manual diagnostic dispatch is not a recovery/publishing command.
4. Verify all five version/target entries after indexing and record the recovery
   evidence on [#638](https://github.com/luthersystems/elps/issues/638).

## Current evidence for #638

The original [v1.61.0](https://github.com/luthersystems/elps/actions/runs/34292740674)
and [v1.61.1](https://github.com/luthersystems/elps/actions/runs/34308282015) failures
stop at `/_apis/gallery` discovery, before the extension lookup/upload completes.
Both time out at 180 seconds; binary builds and packaging succeed. The same ARM
workflow successfully published v1.60.0. The relevant `vsce` 3.9.2,
`azure-devops-node-api` 12.5.0 and transport dependency pins did not change.

On September 9, 2026, an unauthenticated local probe returned discovery HTTP 401
and public-query HTTP 200; the public inventory contained all five v1.60.0
targets and no v1.61.0/v1.61.1 targets. This is **not runner-side or authenticated
recovery evidence**. #638 remains open until Marketplace publication is actually
recovered. The diagnostic workflow supplies the missing runner-side evidence
without turning an investigation into an accidental release.

The [September 10 UTC runner comparison](https://github.com/luthersystems/elps/actions/runs/34444476770)
then reproduced the checks on both hosted fleets: raw discovery returned 401,
the public query returned 200, and the locked SDK returned 401 in 67 ms on ARM
and 163 ms on x64. The inventory still ended at 1.60.0. All three production
build/publish jobs were skipped. There is no evidence from this run of a general
runner-connectivity or unauthenticated SDK failure; authenticated requests remain
untested, so neither changing hosts nor declaring the outage resolved is justified.
The one-off x64 comparison has been removed from the shipped workflow: future
diagnostics stay on the production-equivalent ARM host, with no fleet exception.
