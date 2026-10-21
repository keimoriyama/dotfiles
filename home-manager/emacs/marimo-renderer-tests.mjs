import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createBridge } from "./marimo-renderer.mjs";

test("loopback renderer routes state and UI requests only to the active session", async () => {
  const assets = await mkdtemp(join(tmpdir(), "marimo-assets-"));
  let bridge;
  try {
    await assert.rejects(createBridge({ assets }), /Missing/);
    await writeFile(join(assets, "renderer.mjs"), "export const activate = () => {};");
    const messages = [];
    bridge = await createBridge({ assets, emit(message) {
      messages.push(message);
      bridge.update({ type: "reply", requestId: message.requestId });
    } });
    assert.equal((await fetch(new URL("/cell", bridge.url))).status, 404);
    assert.match(await (await fetch(`${bridge.url}cell?cell=a`)).text(), /marimoHost/);
    assert.match(await (await fetch(`${bridge.url}vendor/renderer.mjs`)).text(), /activate/);
    assert.equal((await fetch(`${bridge.url}vendor/missing.mjs`)).status, 404);
    assert.equal((await fetch(`${bridge.url}vendor/%2e%2e%2fsecret.mjs`)).status, 404);

    const state = { type: "state", cellId: "a", sessionId: "session", state: { output: { data: "42" } } };
    bridge.update(state);
    const first = await (await fetch(`${bridge.url}events?cell=a`)).json();
    assert.deepEqual(first.messages, [state]);
    bridge.update({ type: "notification", sessionId: "session", notification: { op: "function-call-result" } });
    const second = await (await fetch(`${bridge.url}events?cell=a&after=${first.cursor}`)).json();
    assert.equal(second.messages[0].notification.op, "function-call-result");

    const post = (message) => fetch(`${bridge.url}request`, {
      method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(message),
    });
    const request = { cellId: "a", sessionId: "session", command: "invoke-function", params: { functionName: "page" } };
    assert.equal((await post(request)).status, 200);
    assert.equal(messages[0].event, "request");
    assert.deepEqual(messages[0].params, request.params);
    assert.equal((await post({ ...request, sessionId: "old" })).status, 400);
    assert.equal((await post({ ...request, command: "execute" })).status, 400);
    assert.equal((await post({ ...request, cellId: "other" })).status, 400);
    bridge.update({ type: "reset" });
    const last = await (await fetch(`${bridge.url}events?cell=a&after=${second.cursor}`)).json();
    assert.equal(last.messages[0].type, "remove");
    assert.equal((await post(request)).status, 400);
    // Each poll completes, so many cells cannot exhaust six browser connections.
    const many = Array.from({ length: 12 }, (_, i) => `cell-${i}`);
    for (const id of many) bridge.update({ ...state, cellId: id });
    const polls = await Promise.all(many.map(async (id) => (await fetch(`${bridge.url}events?cell=${id}`)).json()));
    assert.deepEqual(polls.map((poll) => poll.messages[0].cellId), many);
    for (let i = 0; i < 260; i++) bridge.update({ ...state, cellId: "newest" });
    const resumed = await (await fetch(`${bridge.url}events?cell=newest&after=1`)).json();
    assert.equal(resumed.messages.length, 1);
    assert.equal(resumed.messages[0].cellId, "newest");
  } finally {
    await bridge?.close();
    await rm(assets, { recursive: true, force: true });
  }
});
