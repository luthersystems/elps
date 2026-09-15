// Narrow adapter for the installed companion's job files and Linux process table.
import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';

const [command, workspace, output, companion] = process.argv.slice(2);
const canonical = p => fs.realpathSync(p);

// Every publication is immutable. Failed/interrupted copies never replace current.
// These are best-effort file snapshots, not transactions across an active editor.
function snapshot() {
  const snapshots = path.join(output, 'snapshots');
  fs.mkdirSync(snapshots, { recursive: true });
  const staging = fs.mkdtempSync(path.join(snapshots, '.partial-'));
  const link = path.join(output, '.current-' + path.basename(staging));
  const git = (args, destination = path.join(staging, '.git-stdout')) => {
    const errors = path.join(staging, '.git-stderr');
    const stdout = fs.openSync(destination, 'w'), stderr = fs.openSync(errors, 'w');
    let result;
    try {
      result = spawnSync('git', ['-C', workspace, ...args], {
        env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' }, stdio: ['ignore', stdout, stderr]
      });
    } finally { fs.closeSync(stdout); fs.closeSync(stderr); }
    const diagnostic = fs.readFileSync(errors, 'utf8');
    fs.unlinkSync(errors);
    if (result.error || result.status !== 0) throw new Error(`Snapshot git ${args[0]} failed: ${result.error?.message || diagnostic}`);
    if (destination !== path.join(staging, '.git-stdout')) return;
    const value = fs.readFileSync(destination, 'utf8');
    fs.unlinkSync(destination);
    return value;
  };
  try {
    const head = git(['rev-parse', 'HEAD']).trim();
    fs.writeFileSync(path.join(staging, 'base.commit'), head + '\n');
    git(['diff', '--binary', '--no-ext-diff', '--no-textconv', head, '--', '.', ':!.codex-delegate'], path.join(staging, 'tracked.patch'));
    const paths = git(['ls-files', '--others', '--exclude-standard', '-z', '--', '.', ':!.codex-delegate']);
    fs.writeFileSync(path.join(staging, 'untracked.paths0'), paths);
    const files = paths.split('\0').filter(Boolean);
    fs.writeFileSync(path.join(staging, 'untracked.list'), files.map(f => JSON.stringify(f) + '\n').join(''));
    fs.mkdirSync(path.join(staging, 'untracked'));
    for (const file of files) {
      const source = path.join(workspace, file), dest = path.join(staging, 'untracked', file);
      fs.mkdirSync(path.dirname(dest), { recursive: true });
      const before = fs.lstatSync(source, { bigint: true });
      if (before.isSymbolicLink()) fs.symlinkSync(fs.readlinkSync(source), dest);
      else if (before.isFile()) {
        fs.copyFileSync(source, dest, fs.constants.COPYFILE_FICLONE);
        fs.chmodSync(dest, Number(before.mode & 0o777n));
      } else throw new Error(`Cannot snapshot non-file ${file}`);
      const after = fs.lstatSync(source, { bigint: true });
      if (before.ino !== after.ino || before.size !== after.size || before.mtimeNs !== after.mtimeNs || before.ctimeNs !== after.ctimeNs) {
        throw new Error(`File changed during snapshot: ${file}; retaining previous snapshot`);
      }
    }
    if (git(['rev-parse', 'HEAD']).trim() !== head) throw new Error('HEAD changed during snapshot; retaining previous snapshot');
    // Hash and flush the captured files, never the live sources. Identical polls
    // cost reads/copies but do not accumulate another full bundle on disk.
    const hash = createHash('sha256');
    function flush(dir) {
      for (const name of fs.readdirSync(dir).sort()) {
        const file = path.join(dir, name), stat = fs.lstatSync(file);
        hash.update(path.relative(staging, file) + '\0' + stat.mode + '\0');
        if (stat.isDirectory()) flush(file);
        else if (stat.isSymbolicLink()) hash.update(fs.readlinkSync(file) + '\0');
        else {
          const fd = fs.openSync(file, 'r');
          try {
            const buffer = Buffer.alloc(65536);
            for (let n; (n = fs.readSync(fd, buffer)) > 0;) hash.update(buffer.subarray(0, n));
            fs.fsyncSync(fd);
          } finally { fs.closeSync(fd); }
        }
      }
      const fd = fs.openSync(dir, 'r');
      try { fs.fsyncSync(fd); } finally { fs.closeSync(fd); }
    }
    flush(staging);
    const digest = hash.digest('hex');
    const current = path.join(output, 'current');
    const destination = path.join(snapshots, 'snapshot-' + digest);
    if (!fs.existsSync(destination)) fs.renameSync(staging, destination);
    else fs.rmSync(staging, { recursive: true });
    fs.symlinkSync(path.relative(output, destination), link);
    fs.renameSync(link, current);
    for (const name of ['tracked.patch', 'base.commit', 'untracked.paths0', 'untracked.list', 'untracked']) {
      const alias = path.join(output, name);
      if (!fs.existsSync(alias)) fs.symlinkSync('current/' + name, alias);
    }
    for (const dir of [snapshots, output]) {
      const fd = fs.openSync(dir, 'r');
      try { fs.fsyncSync(fd); } finally { fs.closeSync(fd); }
    }
  } catch (error) {
    fs.rmSync(link, { force: true });
    fs.rmSync(staging, { recursive: true, force: true });
    throw error;
  }
}

// A standalone reap must obey the same lock as launch/sweep. Reuse fd 9 only
// when it is the actual workspace lock; flock locks the shared open description.
function lockReap() {
  const root = path.join(workspace, '.codex-delegate');
  fs.mkdirSync(root, { recursive: true });
  const lockPath = path.join(root, 'run.lock');
  const fd = fs.openSync(lockPath, 'a');
  let lockFd = fd;
  try {
    const inherited = fs.fstatSync(9), expected = fs.fstatSync(fd);
    if (inherited.dev === expected.dev && inherited.ino === expected.ino) lockFd = 9;
  } catch (error) {
    if (error.code !== 'EBADF') throw error;
  }
  const stdio = ['ignore', 'ignore', 2, 'ignore', 'ignore', 'ignore', 'ignore', 'ignore', 'ignore', lockFd];
  const result = spawnSync('flock', ['-n', '9'], { stdio });
  if (result.error || result.status !== 0) throw new Error(`Workspace run.lock is owned; refusing concurrent reap: ${result.error?.message || result.stderr}`);
  // Keep the descriptor open until process exit, including all asynchronous waits.
}

function scan() {
  const matches = [];
  const unreadable = [];
  let inspected = 0;
  let vanished = 0;
  for (const pid of fs.readdirSync('/proc').filter(p => /^\d+$/.test(p))) {
    try {
      const argv = fs.readFileSync(`/proc/${pid}/cmdline`).toString().split('\0');
      const exe = path.basename(fs.readlinkSync(`/proc/${pid}/exe`));
      const program = path.basename(argv[0]);
      const broker = /^node(?:js)?$/.test(exe) && path.basename(argv[1] || '') === 'app-server-broker.mjs';
      const sandbox = /^codex(?:-.*)?$/.test(exe) && program === 'codex-linux-sandbox';
      const server = /^codex(?:-.*)?$/.test(exe) && argv[1] === 'app-server';
      const worker = /^node(?:js)?$/.test(exe) && path.basename(argv[1] || '') === 'codex-companion.mjs' && ['task', 'task-worker'].includes(argv[2]);
      if (!broker && !sandbox && !server && !worker) {
        inspected++;
        continue;
      }
      const cwd = fs.readlinkSync(`/proc/${pid}/cwd`);
      const roots = [cwd];
      for (const flag of ['--cwd', '--sandbox-policy-cwd', '--command-cwd']) {
        const i = argv.indexOf(flag);
        if (i >= 0 && argv[i + 1]) roots.push(path.resolve(cwd, argv[i + 1]));
      }
      if (roots.some(p => canonical(p) === workspace)) {
        const kind = broker ? 'Codex broker' : sandbox ? 'Codex sandbox' : server ? 'Codex app-server' : 'Codex task worker';
        matches.push({ pid: Number(pid), kind, program,
          line: `PID ${pid}: ${kind} (${program}), workspace ${workspace}. Stop the owning task/session, then verify and stop this exact PID with: kill -TERM ${pid}` });
      }
      inspected++;
    } catch (error) {
      // Processes can exit between directory enumeration and reading their files.
      if (error.code === 'ENOENT' || error.code === 'ESRCH') {
        vanished++;
        continue;
      }
      if (error.code === 'EACCES' || error.code === 'EPERM') {
        unreadable.push(`PID ${pid}: ${error.message}`);
        continue;
      }
      throw new Error(`Cannot inspect PID ${pid}: ${error.message}. Run from a host with access to the process table.`);
    }
  }
  console.error(`[codex-delegate] Process scan: inspected ${inspected}; skipped ${unreadable.length} uninspectable; vanished/unavailable ${vanished}.`);
  if (unreadable.length) {
    console.error(`[codex-delegate] WARNING: Incomplete process visibility; skipped ${unreadable.length} uninspectable processes (permission denied):\n${unreadable.join('\n')}\n[codex-delegate] Uninspectable processes cannot be ruled out as workspace writers. The workspace lock remains required.`);
  }
  return { matches, unreadable };
}

function guard() {
  const { matches } = scan();
  if (matches.length) {
    console.error(matches.map(match => match.line).join('\n'));
    throw new Error('Workspace is occupied. Wait for its broker and sandboxes to stop, use another worktree, or explicitly pass --allow-busy.');
  }
  console.error('[codex-delegate] No workspace writer found among inspected processes.');
}

async function reap() {
  lockReap();
  // A stopping worker can spawn a broker after the first scan. Drain successive
  // generations under the lock; never hand an unfinished teardown to launch.
  for (let generation = 0; generation < 8; generation++) {
    const { matches } = scan();
    if (!matches.length) return;
    // Scan visibility warnings do not establish a writer; only identified targets
    // can fail teardown, just as only identified writers make guard refuse.
    let failed = false;
    function state(match) {
      try {
        const fields = fs.readFileSync(`/proc/${match.pid}/stat`, 'utf8').split(') ').pop().split(' ');
        // Zombies cannot write; start time prevents signaling a reused PID.
        return fields[0] === 'Z' ? null : fields[19];
      } catch (error) {
        if (error.code === 'ENOENT' || error.code === 'ESRCH') return null;
        console.error(`[codex-delegate] Cannot inspect PID ${match.pid} (${match.kind}) during reap: ${error.message}`);
        failed = true;
        return null;
      }
    }
    const targets = matches.map(match => ({ ...match, start: state(match) }));
    const alive = match => match.start !== null && state(match) === match.start;
    function signal(match, name) {
      if (!alive(match)) return;
      console.error(`[codex-delegate] Sending ${name} to PID ${match.pid} (${match.kind}).`);
      try { process.kill(match.pid, name); }
      catch (error) {
        if (error.code === 'ESRCH') return;
        console.error(`[codex-delegate] Cannot send ${name} to PID ${match.pid} (${match.kind}): ${error.message}`);
        failed = true;
      }
    }
    async function waitForExit(milliseconds) {
      const deadline = performance.now() + milliseconds;
      let remaining = targets.filter(alive);
      while (remaining.length && performance.now() < deadline) {
        await new Promise(resolve => setTimeout(resolve, 50));
        remaining = remaining.filter(alive);
      }
      return remaining;
    }
    for (const match of targets) signal(match, 'SIGTERM');
    for (const match of await waitForExit(2000)) signal(match, 'SIGKILL');
    const remaining = await waitForExit(1000);
    for (const match of remaining) console.error(match.line);
    if (failed || remaining.length) {
      throw new Error('Workspace teardown could not be verified. Inspect the PID-specific diagnostics above before retrying.');
    }
  }
  throw new Error('Workspace keeps spawning writers during teardown; refusing launch.');
}

async function monitor() {
  const launch = JSON.parse(fs.readFileSync(path.join(output, 'launch.json'), 'utf8'));
  if (!launch.jobId || !launch.logFile) throw new Error('Plugin launch lacks jobId/logFile. Check the installed companion version and launch.json.');
  const { resolveJobFile } = await import(pathToFileURL(path.join(path.dirname(companion), 'lib/state.mjs')));
  const transcript = path.join(output, 'transcript.log');
  let offset = 0;
  let stdoutOpen = true;
  process.stdout.on('error', error => {
    if (error.code !== 'EPIPE') throw error;
    stdoutOpen = false;
    console.error('Caller closed stdout; capture continues in ' + transcript);
  });
  function drain() {
    const fd = fs.openSync(launch.logFile, 'r');
    try {
      const size = fs.fstatSync(fd).size;
      if (size < offset) throw new Error('Plugin log was truncated. Inspect the job log before retrying.');
      const buffer = Buffer.alloc(65536);
      while (offset < size) {
        const count = fs.readSync(fd, buffer, 0, Math.min(buffer.length, size - offset), offset);
        if (!count) throw new Error('Plugin log changed during capture. Inspect the job log.');
        const chunk = buffer.subarray(0, count);
        // Disk writes happen immediately, independently of downstream stdout pipes.
        fs.appendFileSync(transcript, chunk);
        if (stdoutOpen) process.stdout.write(chunk);
        offset += count;
      }
    } finally { fs.closeSync(fd); }
  }
  let lastSnapshot = 0, invalidSince = null;
  for (;;) {
    if (performance.now() - lastSnapshot >= 1000 || lastSnapshot === 0) {
      try { snapshot(); }
      catch (error) { console.error(`[codex-delegate] Snapshot failed; previous captures retained: ${error.message}`); }
      lastSnapshot = performance.now();
    }
    drain();
    let job;
    try {
      job = JSON.parse(fs.readFileSync(resolveJobFile(workspace, launch.jobId), 'utf8'));
      invalidSince = null;
    } catch (error) {
      if (!(error instanceof SyntaxError) && error.code !== 'ENOENT') throw error;
      invalidSince ??= performance.now();
      console.error(`[codex-delegate] Job state temporarily unreadable: ${error.message}`);
      if (performance.now() - invalidSince > 2000) throw new Error('Job state remained unreadable for 2 seconds; inspect plugin job file.');
      await new Promise(resolve => setTimeout(resolve, 100));
      continue;
    }
    if (['completed', 'failed', 'canceled', 'cancelled'].includes(job.status)) {
      drain();
      fs.writeFileSync(path.join(output, 'job.json'), JSON.stringify(job, null, 2) + '\n');
      const final = job.result?.rawOutput;
      if (typeof final === 'string') fs.writeFileSync(path.join(output, 'final-message.txt'), final);
      else {
        fs.writeFileSync(path.join(output, 'final-message.txt'), '');
        console.error(`No final message was produced: ${job.errorMessage || job.status}. Inspect job.json and transcript.log.`);
        if (job.status === 'completed') throw new Error('Completed job has no rawOutput field. Check plugin result wiring in job.json.');
      }
      if (job.status !== 'completed') {
        console.error(`Codex job ${launch.jobId} ${job.status}: ${job.errorMessage || 'Inspect job.json and transcript.log before retrying.'}`);
        process.exitCode = 3;
      }
      return;
    }
    if (!['queued', 'running'].includes(job.status) || !Number.isInteger(job.pid)) {
      throw new Error(`Invalid job state ${job.status}/PID ${job.pid}. Inspect the plugin job file before retrying.`);
    }
    try { process.kill(job.pid, 0); }
    catch (error) { throw new Error(`Cannot reach job ${launch.jobId} worker PID ${job.pid}: ${error.message}. Inspect the plugin job and stop remaining sandboxes before retrying.`); }
    const state = fs.readFileSync(`/proc/${job.pid}/stat`, 'utf8').split(') ').pop().split(' ')[0];
    if (state === 'Z') throw new Error(`Job worker ${job.pid} exited without a terminal result. Inspect the plugin job and remaining sandboxes before retrying.`);
    await new Promise(resolve => setTimeout(resolve, 250));
  }
}

try {
  if (command === 'snapshot') snapshot();
  else if (command === 'guard') guard();
  else if (command === 'reap') await reap();
  else if (command === 'monitor') await monitor();
  else throw new Error(`Unknown adapter command ${command}. Use snapshot, guard, reap or monitor.`);
} catch (error) {
  console.error(`[codex-delegate] ERROR: ${error.message}`);
  process.exitCode = 3;
}
