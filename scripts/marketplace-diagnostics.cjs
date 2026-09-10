'use strict';

// Read-only, unauthenticated diagnostics for #638. Never print headers, response
// bodies, environment variables or exception messages. A reachable service is
// not evidence that the publishing PAT works or that an upload will succeed.
const http = require('node:http');
const https = require('node:https');
const { execFile } = require('node:child_process');
const { performance } = require('node:perf_hooks');
const { isIP } = require('node:net');

const MARKETPLACE = 'https://marketplace.visualstudio.com';
const TARGETS = new Set(['universal', 'linux-x64', 'linux-arm64', 'darwin-x64', 'darwin-arm64',
  'win32-x64', 'win32-arm64', 'win32-ia32', 'linux-armhf', 'alpine-x64', 'alpine-arm64', 'web']);
const QUERY = JSON.stringify({ filters: [{ criteria: [{ filterType: 7, value: 'LutherSystems.elps-lang' }] }], flags: 1 });

function compareVersions(left, right) {
  const a = left.split('.');
  const b = right.split('.');
  for (let i = 0; i < 3; i++) {
    // Inputs are validated digit triplets. Length then lexical comparison is
    // numeric without rounding components larger than Number.MAX_SAFE_INTEGER.
    const x = a[i].replace(/^0+(?=\d)/, '');
    const y = b[i].replace(/^0+(?=\d)/, '');
    if (x.length !== y.length) return x.length - y.length;
    if (x !== y) return x < y ? -1 : 1;
  }
  return left.localeCompare(right);
}

function versionInventory(body) {
  const extensions = body.results?.flatMap(result => result.extensions ?? []);
  const extension = extensions?.find(value => value.publisher?.publisherName?.toLowerCase() === 'luthersystems'
    && value.extensionName === 'elps-lang');
  if (!Array.isArray(extension?.versions) || extension.versions.length === 0) throw new Error('INVALID_INVENTORY');
  return extension.versions.map(value => {
    const target = value.targetPlatform == null ? 'universal' : value.targetPlatform;
    if (typeof value.version !== 'string' || !/^\d+\.\d+\.\d+$/.test(value.version)
      || typeof target !== 'string' || !TARGETS.has(target)) throw new Error('INVALID_INVENTORY');
    return { version: value.version, target };
  }).sort((a, b) => compareVersions(a.version, b.version) || a.target.localeCompare(b.target));
}

function observe(url, method, { timeoutMs = 15000, maxBytes = 1048576, inventory = false } = {}) {
  return new Promise(resolve => {
    const started = performance.now();
    const result = { bytes: 0, timing: {} };
    const stamp = name => { result.timing[name] = Math.round(performance.now() - started); };
    let finished = false;
    let timer;
    const finish = error => {
      if (finished) return;
      finished = true;
      clearTimeout(timer);
      if (error) result.error = error;
      stamp('totalMs');
      resolve(result);
    };
    const request = (url.startsWith('https:') ? https : http).request(url, {
      method,
      headers: inventory ? { 'Content-Type': 'application/json', Accept: 'application/json;api-version=7.2-preview.1' } : {},
    }, response => {
      result.status = response.statusCode;
      stamp('firstByteMs');
      const chunks = [];
      response.on('data', chunk => {
        result.bytes += chunk.length;
        if (result.bytes > maxBytes) {
          finish('BODY_LIMIT');
          request.destroy();
        } else if (inventory) chunks.push(chunk);
      });
      response.on('error', () => finish('RESPONSE_ERROR'));
      response.on('end', () => {
        if (finished) return;
        if (inventory) {
          if (result.status !== 200) return finish('HTTP_STATUS');
          try { result.versions = versionInventory(JSON.parse(Buffer.concat(chunks).toString())); }
          catch { return finish('INVALID_INVENTORY'); }
        }
        finish();
      });
    });
    request.on('socket', socket => {
      socket.on('lookup', (error, address, family) => {
        stamp('dnsMs');
        if (!error && isIP(address)) result.dns = { address, family };
      });
      socket.on('connect', () => stamp('connectMs'));
      socket.on('secureConnect', () => stamp('tlsMs'));
    });
    request.on('error', () => finish('REQUEST_ERROR'));
    // An absolute deadline, not socket inactivity: a slow stream cannot extend it.
    timer = setTimeout(() => { finish('TIMEOUT'); request.destroy(); }, timeoutMs);
    request.end(inventory ? QUERY : undefined);
  });
}

async function azureClientProbe(endpoint = MARKETPLACE) {
  // Same locked API client and call that vsce 3.9.2 uses before uploading. No
  // BasicAuth handler: deliberately cannot test the secret's validity/permissions.
  const { GalleryApi } = require('../editors/vscode/node_modules/azure-devops-node-api/GalleryApi');
  const api = new GalleryApi(endpoint, [], { socketTimeout: 15000, allowRedirects: false });
  const started = performance.now();
  let status;
  try { await api.getExtension(null, 'LutherSystems', 'elps-lang', undefined, 1); status = 'OK'; }
  catch (error) {
    status = Number.isInteger(error.statusCode) ? `HTTP_${error.statusCode}` : 'CLIENT_ERROR';
  }
  return { status, elapsedMs: Math.round(performance.now() - started) };
}

function observeAzureClient({ args = [__filename, '--azure-client'], timeoutMs = 20000 } = {}) {
  return new Promise(resolve => {
    // The subprocess bound covers DNS, TLS, SDK retries and SDK import, not just
    // one socket. Empty environment and no handlers keep this probe unauthenticated.
    execFile(process.execPath, args, { timeout: timeoutMs, maxBuffer: 4096, env: {} }, (error, stdout) => {
      if (error) return resolve({ status: error.code === 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER' ? 'OUTPUT_LIMIT'
        : error.killed ? 'TIMEOUT' : 'CLIENT_FAILED' });
      try {
        const value = JSON.parse(stdout);
        if (typeof value.status !== 'string' || !/^(OK|HTTP_\d{3}|CLIENT_ERROR)$/.test(value.status)
          || !Number.isInteger(value.elapsedMs) || value.elapsedMs < 0) throw new Error();
        resolve({ status: value.status, elapsedMs: value.elapsedMs });
      } catch { resolve({ status: 'INVALID_CLIENT_RESULT' }); }
    });
  });
}

async function main() {
  const [discovery, inventory, azureClient] = await Promise.all([
    observe(`${MARKETPLACE}/_apis/gallery`, 'OPTIONS'),
    observe(`${MARKETPLACE}/_apis/public/gallery/extensionquery`, 'POST', { inventory: true }),
    observeAzureClient(),
  ]);
  console.log(JSON.stringify({ node: process.version, architecture: process.arch, discovery, inventory, azureClient }, null, 2));
  // Network failures are evidence, not a CI gate. The manual workflow's log is a
  // diagnostic report; successful execution must not be called publication success.
}

module.exports = { observe, versionInventory, azureClientProbe, observeAzureClient };
if (require.main === module) {
  (process.argv[2] === '--azure-client'
    ? azureClientProbe().then(value => process.stdout.write(JSON.stringify(value)))
    : main()).catch(() => {
    console.error('Diagnostic program failed; no response content was logged.');
    process.exitCode = 1;
  });
}
