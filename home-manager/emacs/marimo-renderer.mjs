// Transport adapter for the unmodified, version-pinned marimo VS Code renderer.
import http from "node:http";
import { readFile } from "node:fs/promises";
import { createReadStream, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { randomBytes } from "node:crypto";
import { createInterface } from "node:readline";

const commands = new Set(["invoke-function", "update-ui-element", "set-model-value",
  "resize", "renderer-error", "navigate-to-cell", "copy-image", "save-image"]);

export async function createBridge({ assets, emit = () => {} }) {
  if (!existsSync(join(assets, "renderer.mjs"))) throw new Error("Missing marimo renderer assets");
  const token = randomBytes(24).toString("hex");
  const prefix = `/${token}/`;
  const states = new Map();
  const history = [];
  let revision = 0;
  const pending = new Map();
  let nextRequest = 0;
  const html = await readFile(join(dirname(fileURLToPath(import.meta.url)), "marimo-renderer.html"));

  function broadcast(cellId, message) {
    history.push({ revision: ++revision, cellId, message });
    if (history.length > 256) history.shift();
  }

  function update(message) {
    if (message.type === "state") {
      states.set(message.cellId, message);
      broadcast(message.cellId, message);
    } else if (message.type === "notification") {
      broadcast(null, message);
    } else if (message.type === "remove") {
      states.delete(message.cellId);
      broadcast(message.cellId, { type: "remove" });
    } else if (message.type === "reset") {
      for (const id of states.keys()) broadcast(id, { type: "remove" });
      states.clear();
    } else if (message.type === "reply") {
      const request = pending.get(message.requestId);
      if (request) {
        clearTimeout(request.timer);
        pending.delete(message.requestId);
        request.response.writeHead(message.error ? 400 : 200, { "Content-Type": "application/json" });
        request.response.end(JSON.stringify(message.error ? { error: message.error } : null));
      }
    }
  }

  const server = http.createServer(async (request, response) => {
    try {
      const url = new URL(request.url, "http://localhost");
      if (!url.pathname.startsWith(prefix)) { response.writeHead(404).end(); return; }
      const path = url.pathname.slice(prefix.length);
      response.setHeader("Cache-Control", "no-store");
      response.setHeader("X-Content-Type-Options", "nosniff");
      if (request.method === "GET" && path.startsWith("vendor/")) {
        const filename = path.slice(7);
        if (!/^[a-zA-Z0-9_.-]+\.(mjs|css|woff2?|ttf|svg|png|jpg|wasm)$/.test(filename)) {
          response.writeHead(404).end(); return;
        }
        const file = join(assets, filename);
        if (!existsSync(file)) { response.writeHead(404).end(); return; }
        const mime = filename.endsWith(".mjs") ? "text/javascript"
          : filename.endsWith(".css") ? "text/css" : filename.endsWith(".svg") ? "image/svg+xml"
          : filename.endsWith(".wasm") ? "application/wasm" : "application/octet-stream";
        response.setHeader("Cache-Control", "private, max-age=31536000, immutable");
        response.writeHead(200, { "Content-Type": mime });
        createReadStream(file).pipe(response);
      } else if (request.method === "GET" && path === "cell") {
        response.writeHead(200, { "Content-Type": "text/html; charset=utf-8" }).end(html);
      } else if (request.method === "GET" && path === "events") {
        const id = url.searchParams.get("cell");
        const after = Number(url.searchParams.get("after") || 0);
        const messages = !after || after < (history[0]?.revision ?? 0) - 1
          ? (states.has(id) ? [states.get(id)] : [{ type: "remove" }])
          : history.filter((event) => event.revision > after && (!event.cellId || event.cellId === id))
            .map((event) => event.message);
        response.writeHead(200, { "Content-Type": "application/json" })
          .end(JSON.stringify({ cursor: revision, messages }));
      } else if (request.method === "POST" && path === "request") {
        let body = "";
        for await (const chunk of request) {
          body += chunk;
          if (body.length > 8 * 1024 * 1024) throw new Error("Renderer request too large");
        }
        const message = JSON.parse(body);
        const state = states.get(message.cellId);
        if (!state || message.sessionId !== state.sessionId) throw new Error("Stale kernel session");
        if (!commands.has(message.command)) throw new Error("Unsupported renderer command");
        const requestId = ++nextRequest;
        const timer = setTimeout(() => {
          pending.delete(requestId);
          response.writeHead(504).end("Renderer request timed out");
        }, 20000);
        pending.set(requestId, { response, timer });
        emit({ ...message, event: "request", requestId });
      } else {
        response.writeHead(404).end();
      }
    } catch (error) {
      if (!response.headersSent) response.writeHead(400, { "Content-Type": "application/json" });
      response.end(JSON.stringify({ error: error.message }));
    }
  });
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", resolve);
  });
  return {
    url: `http://127.0.0.1:${server.address().port}${prefix}`,
    update,
    close() {
      for (const { timer, response } of pending.values()) {
        clearTimeout(timer); response.end();
      }
      server.closeAllConnections();
      return new Promise((resolve) => server.close(resolve));
    },
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const emit = (message) => process.stdout.write(`${JSON.stringify(message)}\n`);
  const bridge = await createBridge({ assets: process.argv[2], emit });
  emit({ event: "ready", url: bridge.url });
  const input = createInterface({ input: process.stdin });
  input.on("line", (line) => {
    try { bridge.update(JSON.parse(line)); }
    catch (error) { emit({ event: "error", message: error.message }); }
  });
  input.on("close", async () => { await bridge.close(); process.exit(0); });
}
