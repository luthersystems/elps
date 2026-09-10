'use strict';

const assert = require('node:assert/strict');
const http = require('node:http');
const { test } = require('node:test');
const { observe, versionInventory, observeAzureClient } = require('./marketplace-diagnostics.cjs');

async function serve(t, handler) {
  const server = http.createServer(handler);
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  t.after(() => { server.closeAllConnections(); server.close(); });
  return `http://127.0.0.1:${server.address().port}`;
}

test('discovery uses OPTIONS without credentials and does not print response content', async t => {
  const url = await serve(t, (req, res) => {
    assert.equal(req.method, 'OPTIONS');
    assert.equal(req.headers.authorization, undefined);
    assert.equal(req.headers.cookie, undefined);
    res.writeHead(401, { 'Set-Cookie': 'private-marker' });
    res.end('private-marker');
  });
  const result = await observe(url, 'OPTIONS');
  assert.equal(result.status, 401);
  assert.equal(result.error, undefined);
  assert.equal(result.bytes, 14);
  assert.equal(JSON.stringify(result).includes('private-marker'), false);
  assert.equal(typeof result.timing.firstByteMs, 'number');
});

test('request has an absolute timeout even when the server keeps sending bytes', { timeout: 1000 }, async t => {
  const url = await serve(t, (_req, res) => {
    res.writeHead(200);
    const ticker = setInterval(() => res.write('x'), 5);
    res.on('close', () => clearInterval(ticker));
  });
  const result = await observe(url, 'GET', { timeoutMs: 50 });
  assert.equal(result.error, 'TIMEOUT');
});

test('socket failure is a diagnostic error, not a successful empty response', async t => {
  const url = await serve(t, req => req.socket.destroy());
  const result = await observe(url, 'OPTIONS');
  assert.equal(result.error, 'REQUEST_ERROR');
  assert.equal(result.status, undefined);
});

test('response body limit aborts a successful HTTP response', async t => {
  const url = await serve(t, (_req, res) => res.end('x'.repeat(100)));
  const result = await observe(url, 'GET', { maxBytes: 32 });
  assert.equal(result.error, 'BODY_LIMIT');
});

test('public query reports only exact matching extension version/target fields', async t => {
  const url = await serve(t, async (req, res) => {
    assert.equal(req.method, 'POST');
    let body = '';
    for await (const chunk of req) body += chunk;
    assert.equal(JSON.parse(body).filters[0].criteria[0].value, 'LutherSystems.elps-lang');
    res.setHeader('Content-Type', 'application/json');
    res.end(JSON.stringify({ results: [{ extensions: [
      { publisher: { publisherName: 'Other' }, extensionName: 'elps-lang', versions: [{ version: '9.0.0' }] },
      { publisher: { publisherName: 'LutherSystems' }, extensionName: 'elps-lang', private: 'private-marker', versions: [
        { version: '1.61.1', targetPlatform: 'linux-arm64', private: 'private-marker' },
        { version: '1.61.1' }, { version: '1.60.0', targetPlatform: 'darwin-x64' },
      ] },
    ] }] }));
  });
  const result = await observe(url, 'POST', { inventory: true });
  assert.deepEqual(result.versions, [
    { version: '1.60.0', target: 'darwin-x64' },
    { version: '1.61.1', target: 'linux-arm64' },
    { version: '1.61.1', target: 'universal' },
  ]);
  assert.equal(JSON.stringify(result).includes('private-marker'), false);
});

test('non-JSON, HTTP errors and wrong schema are failures, never an empty inventory', async t => {
  for (const [status, body, error] of [
    [200, 'not json private-marker', 'INVALID_INVENTORY'],
    [503, '{"error":"private-marker"}', 'HTTP_STATUS'],
    [200, '{"results":[]}', 'INVALID_INVENTORY'],
    [302, '', 'HTTP_STATUS'],
  ]) {
    const url = await serve(t, (_req, res) => { res.writeHead(status); res.end(body); });
    const result = await observe(url, 'POST', { inventory: true });
    assert.equal(result.error, error);
    assert.equal(result.versions, undefined);
    assert.equal(JSON.stringify(result).includes('private-marker'), false);
  }
});

test('inventory rejects unexpected version/target text instead of logging it', () => {
  const extension = { publisher: { publisherName: 'LutherSystems' }, extensionName: 'elps-lang' };
  for (const versions of [[], [{ version: 'secret-value' }], [{ version: ['1.2.3'] }],
    [{ version: '1.2.3', targetPlatform: 'secret-value' }], [{ version: '1.2.3', targetPlatform: false }]]) {
    assert.throws(() => versionInventory({ results: [{ extensions: [{ ...extension, versions }] }] }));
  }
});

test('inventory orders numeric version components before target names, without number rounding', () => {
  const expected = [
    { version: '1.9.0', target: 'universal' },
    { version: '1.10.0', target: 'universal' },
    { version: '1.99.0', target: 'universal' },
    { version: '1.100.0', target: 'universal' },
    { version: '2.0.9', target: 'universal' },
    { version: '2.0.10', target: 'darwin-x64' },
    { version: '2.0.10', target: 'universal' },
    { version: '9.0.0', target: 'universal' },
    { version: '10.0.0', target: 'universal' },
    // Number() rounds both major components to the same value. The opposite
    // target order ensures that losing this distinction fails the assertion.
    { version: '9007199254740992.0.0', target: 'universal' },
    { version: '9007199254740993.0.0', target: 'darwin-x64' },
  ];
  const versions = [...expected].reverse().map(({ version, target }) => ({ version, targetPlatform: target }));
  const body = { results: [{ extensions: [{ publisher: { publisherName: 'LutherSystems' },
    extensionName: 'elps-lang', versions }] }] };
  assert.deepEqual(versionInventory(body), expected);
});

test('SDK subprocess does not inherit environment or expose extra output fields', async t => {
  process.env.ELPS_DIAGNOSTIC_TEST_SENTINEL = 'private-marker';
  t.after(() => { delete process.env.ELPS_DIAGNOSTIC_TEST_SENTINEL; });
  const result = await observeAzureClient({ args: ['-e', `
    console.log(JSON.stringify({
      status: process.env.ELPS_DIAGNOSTIC_TEST_SENTINEL ? 'INHERITED_ENV' : 'OK',
      elapsedMs: 1, private: 'private-marker'
    }));
  `] });
  assert.deepEqual(result, { status: 'OK', elapsedMs: 1 });
});

test('SDK subprocess has an absolute deadline', { timeout: 2000 }, async () => {
  const result = await observeAzureClient({ args: ['-e', 'setInterval(() => {}, 10)'], timeoutMs: 50 });
  assert.deepEqual(result, { status: 'TIMEOUT' });
});

test('SDK subprocess failures and malformed output cannot masquerade as probe results', async () => {
  for (const [script, status] of [
    ['process.exit(1)', 'CLIENT_FAILED'],
    ['console.log("private-marker")', 'INVALID_CLIENT_RESULT'],
    ['console.log(JSON.stringify({status:"private-marker", elapsedMs:1}))', 'INVALID_CLIENT_RESULT'],
    ['console.log(JSON.stringify({status:["OK"], elapsedMs:1}))', 'INVALID_CLIENT_RESULT'],
    ['console.log(JSON.stringify({status:"OK", elapsedMs:-1}))', 'INVALID_CLIENT_RESULT'],
    ['console.log("x".repeat(5000))', 'OUTPUT_LIMIT'],
  ]) {
    assert.deepEqual(await observeAzureClient({ args: ['-e', script] }), { status });
  }
});
