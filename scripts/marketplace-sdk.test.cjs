'use strict';

// Runs after npm ci in manual diagnostics, against the actual locked SDK.
const assert = require('node:assert/strict');
const http = require('node:http');
const { test } = require('node:test');
const { azureClientProbe } = require('./marketplace-diagnostics.cjs');

async function serve(t, handler) {
  const server = http.createServer(handler);
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  t.after(() => { server.closeAllConnections(); server.close(); });
  return `http://127.0.0.1:${server.address().port}`;
}

test('actual SDK makes unauthenticated discovery and sanitizes its error', { timeout: 2000 }, async t => {
  const calls = [];
  const url = await serve(t, (req, res) => {
    calls.push([req.method, req.url, req.headers.authorization, req.headers.cookie]);
    res.writeHead(401, { 'Content-Type': 'application/json' });
    res.end('{"message":"private-marker"}');
  });
  const result = await azureClientProbe(url);
  assert.deepEqual(calls, [['OPTIONS', '/_apis/gallery', undefined, undefined]]);
  assert.equal(result.status, 'HTTP_401');
  assert.equal(JSON.stringify(result).includes('private-marker'), false);
});

test('actual SDK does not follow a discovery redirect to another host', { timeout: 2000 }, async t => {
  let redirectedCalls = 0;
  const redirect = await serve(t, (_req, res) => { redirectedCalls++; res.writeHead(401); res.end(); });
  const url = await serve(t, (_req, res) => { res.writeHead(302, { Location: redirect }); res.end(); });
  await azureClientProbe(url);
  assert.equal(redirectedCalls, 0);
});
