import type { Denops } from "jsr:@denops/std@^7.0.0";
import { system } from "jsr:@denops/std/function";
import { open } from "jsr:@denops/std/buffer";
import { setbufline } from "jsr:@denops/std/function";

import { assertEquals } from "jsr:@std/assert@~1.0.2/equals";

type FrameworkId = string;

type TestResult = {
  framework: FrameworkId;
  filename: string; // absolute path when possible
  displayPath?: string; // nicer path for display
  classname?: string; // raw JUnit classname
  testname: string; // raw JUnit name
  result: boolean; // true: pass, false: fail
  lnum?: number;
  col?: number;
  // If the test failed, try to point at the actual error location (stacktrace/traceback),
  // not just the test definition.
  errorFile?: string;
  errorLnum?: number;
  errorCol?: number;
};

type TestRunSummary = {
  passed: number;
  failed: number;
  durationMs?: number;
};

type ParsedTestRun = {
  results: TestResult[];
  summary?: TestRunSummary;
};

type JunitParseOptions = {
  rootDir: string;
  framework: FrameworkId;
  resolveClassnameToPath?: (classname: string) => string | null;
};

type TestFramework = {
  id: FrameworkId;
  label: string;
  runAll: (denops: Denops) => Promise<ParsedTestRun>;
  runOne?: (denops: Denops, test: TestResult) => Promise<ParsedTestRun>;
};

const RESULT_BUFNAME = "[DenopsTestRunner]";
const RESULT_FILETYPE = "denops-test-runner";

const RESULT_ITEMS_VAR = "denops_test_runner_items";
const RESULT_START_LNUM_VAR = "denops_test_runner_start_lnum";
const RESULT_PREV_WINID_VAR = "denops_test_runner_prev_winid";
const RESULT_FRAMEWORK_VAR = "denops_test_runner_framework";

export async function main(denops: Denops): Promise<void> {
  const rootDir = guessPluginRootDir();

  const frameworks = new Map<FrameworkId, TestFramework>();
  const denoFw = new DenoTestFramework(rootDir);
  const pytestFw = new PytestFramework(rootDir);
  frameworks.set(denoFw.id, denoFw);
  frameworks.set(pytestFw.id, pytestFw);

  const defaultFrameworkId: FrameworkId = "deno";
  const ui = new ResultBufferUI(denops.name, frameworks, defaultFrameworkId);

  denops.dispatcher = {
    async test_run(frameworkId?: unknown): Promise<void> {
      await ui.runAllAndShow(
        denops,
        typeof frameworkId === "string" ? frameworkId : undefined,
      );
    },
    async rerun_all(): Promise<void> {
      await ui.rerunAllFromCurrentBuffer(denops);
    },
    async open_item(lnum?: unknown): Promise<void> {
      await ui.openItemUnderCursor(denops, lnum);
    },
    async rerun_item(lnum?: unknown): Promise<void> {
      await ui.rerunItemUnderCursor(denops, lnum);
    },
  };

  // `:Testrun [framework]` where framework is one of: deno, pytest
  await denops.cmd(
    `command! -nargs=? Testrun call denops#request('${denops.name}', 'test_run', [<q-args>])`,
  );
  await denops.cmd(
    `command! TestrunOpen call denops#request('${denops.name}', 'open_item', [line('.')])`,
  );
  await denops.cmd(
    `command! TestrunRerun call denops#request('${denops.name}', 'rerun_item', [line('.')])`,
  );
}

class DenoTestFramework implements TestFramework {
  id = "deno";
  label = "deno test";
  #rootDir: string;

  constructor(rootDir: string) {
    this.#rootDir = rootDir;
  }

  async runAll(denops: Denops): Promise<ParsedTestRun> {
    const script = `cd ${
      shellQuote(this.#rootDir)
    } && deno test -A --doc --parallel --shuffle --no-check --reporter junit denops/`;
    const raw = await system(denops, `sh -lc ${shellQuote(script)}`);
    return parseJunit(raw, {
      rootDir: this.#rootDir,
      framework: this.id,
      resolveClassnameToPath: (c) =>
        guessPathFromJunitClassname(this.#rootDir, c),
    });
  }

  async runOne(denops: Denops, test: TestResult): Promise<ParsedTestRun> {
    const filterRegex = `^${escapeForRegex(test.testname)}$`;
    const absFile = test.filename;
    const script = `cd ${
      shellQuote(this.#rootDir)
    } && deno test -A --doc --no-check --filter ${
      shellQuote(filterRegex)
    } --reporter junit ${shellQuote(absFile)}`;
    const raw = await system(denops, `sh -lc ${shellQuote(script)}`);
    return parseJunit(raw, {
      rootDir: this.#rootDir,
      framework: this.id,
      resolveClassnameToPath: (c) =>
        guessPathFromJunitClassname(this.#rootDir, c),
    });
  }
}

class PytestFramework implements TestFramework {
  id = "pytest";
  label = "pytest";
  #rootDir: string;

  constructor(rootDir: string) {
    this.#rootDir = rootDir;
  }

  async runAll(denops: Denops): Promise<ParsedTestRun> {
    const script = [
      `cd ${shellQuote(this.#rootDir)}`,
      `tmp="$(mktemp -t pytest-junit.XXXXXX)"`,
      `python -m pytest -q --disable-warnings --junitxml "$tmp"`,
      `cat "$tmp"`,
      `rm -f "$tmp"`,
    ].join(" && ");
    const raw = await system(denops, `sh -lc ${shellQuote(script)}`);
    return parseJunit(raw, {
      rootDir: this.#rootDir,
      framework: this.id,
      resolveClassnameToPath: (c) => resolvePytestClassname(this.#rootDir, c),
    });
  }

  async runOne(denops: Denops, test: TestResult): Promise<ParsedTestRun> {
    // Best-effort rerun for pytest:
    // - if we have a real file path, pass it
    // - filter by test name using -k (substring/expression match)
    const filePart = test.filename && test.filename.endsWith(".py") &&
        fileExists(test.filename)
      ? ` ${shellQuote(test.filename)}`
      : "";

    const script = [
      `cd ${shellQuote(this.#rootDir)}`,
      `tmp="$(mktemp -t pytest-junit.XXXXXX)"`,
      `python -m pytest -q --disable-warnings -k ${
        shellQuote(test.testname)
      } --junitxml "$tmp"${filePart}`,
      `cat "$tmp"`,
      `rm -f "$tmp"`,
    ].join(" && ");
    const raw = await system(denops, `sh -lc ${shellQuote(script)}`);
    return parseJunit(raw, {
      rootDir: this.#rootDir,
      framework: this.id,
      resolveClassnameToPath: (c) => resolvePytestClassname(this.#rootDir, c),
    });
  }
}

class ResultBufferUI {
  #pluginName: string;
  #frameworks: Map<FrameworkId, TestFramework>;
  #defaultFrameworkId: FrameworkId;

  constructor(
    pluginName: string,
    frameworks: Map<FrameworkId, TestFramework>,
    defaultFrameworkId: FrameworkId,
  ) {
    this.#pluginName = pluginName;
    this.#frameworks = frameworks;
    this.#defaultFrameworkId = defaultFrameworkId;
  }

  async runAllAndShow(
    denops: Denops,
    frameworkId?: FrameworkId,
  ): Promise<void> {
    const fw = this.#getFrameworkOrDefault(frameworkId);
    const prevWinid = await denops.call("win_getid") as number;
    const { bufnr } = await open(denops, RESULT_BUFNAME, { opener: "split" });
    await this.#configureBuffer(denops, bufnr, prevWinid);
    await denops.call("setbufvar", bufnr, RESULT_FRAMEWORK_VAR, fw.id);

    await this.#render(
      denops,
      bufnr,
      { results: [] },
      `Running: ${fw.label} ...`,
    );

    const parsed = await fw.runAll(denops);
    await this.#render(denops, bufnr, parsed);
  }

  async rerunAllFromCurrentBuffer(denops: Denops): Promise<void> {
    const bufnr = await denops.call("bufnr", "%") as number;
    const fwId = await denops.call(
      "getbufvar",
      bufnr,
      RESULT_FRAMEWORK_VAR,
      "",
    ) as string;
    const fw = this.#getFrameworkOrDefault(fwId || undefined);

    await this.#render(
      denops,
      bufnr,
      { results: [] },
      `Running: ${fw.label} ...`,
    );
    const parsed = await fw.runAll(denops);
    await this.#render(denops, bufnr, parsed);
  }

  async openItemUnderCursor(
    denops: Denops,
    lnumInput?: unknown,
  ): Promise<void> {
    const bufnr = await denops.call("bufnr", "%") as number;
    const item = await this.#getItemAtLine(denops, bufnr, lnumInput);
    if (!item?.filename) return;

    const jump = pickJumpTarget(item);
    if (!jump.filename) return;

    if (!fileExists(jump.filename)) {
      await denops.cmd(
        `echoerr ${vimString(`Cannot open: file not found: ${jump.filename}`)}`,
      );
      return;
    }

    const prevWinid = await denops.call(
      "getbufvar",
      bufnr,
      RESULT_PREV_WINID_VAR,
      0,
    ) as number;
    if (prevWinid) {
      await denops.call("win_gotoid", prevWinid);
    } else {
      await denops.cmd("wincmd p");
    }

    const escaped = await denops.call("fnameescape", jump.filename) as string;
    await denops.cmd(`keepalt keepjumps edit ${escaped}`);
    if (jump.lnum) {
      await denops.call("cursor", jump.lnum, jump.col ?? 1);
      await denops.cmd("normal! zz");
    }
  }

  async rerunItemUnderCursor(
    denops: Denops,
    lnumInput?: unknown,
  ): Promise<void> {
    const bufnr = await denops.call("bufnr", "%") as number;
    const item = await this.#getItemAtLine(denops, bufnr, lnumInput);
    if (!item) return;

    const fwId = await denops.call(
      "getbufvar",
      bufnr,
      RESULT_FRAMEWORK_VAR,
      "",
    ) as string;
    const fw = this.#getFrameworkOrDefault(fwId || undefined);

    if (!fw.runOne) {
      await this.rerunAllFromCurrentBuffer(denops);
      return;
    }

    await this.#render(
      denops,
      bufnr,
      { results: [] },
      `Running: ${fw.label} (selected) ...`,
    );
    const parsed = await fw.runOne(denops, item);
    await this.#render(denops, bufnr, parsed);
  }

  #getFrameworkOrDefault(frameworkId?: FrameworkId): TestFramework {
    const key = (frameworkId ?? "").trim();
    return this.#frameworks.get(key) ??
      this.#frameworks.get(this.#defaultFrameworkId)!;
  }

  async #getItemAtLine(
    denops: Denops,
    bufnr: number,
    lnumInput?: unknown,
  ): Promise<TestResult | null> {
    const startLnum = await denops.call(
      "getbufvar",
      bufnr,
      RESULT_START_LNUM_VAR,
      1,
    ) as number;
    const items = await denops.call(
      "getbufvar",
      bufnr,
      RESULT_ITEMS_VAR,
      [],
    ) as unknown;

    const lnum = typeof lnumInput === "number"
      ? lnumInput
      : Number(lnumInput ?? await denops.call("line", "."));

    if (!Array.isArray(items)) return null;
    const idx = lnum - startLnum;
    if (idx < 0 || idx >= items.length) return null;
    return items[idx] as TestResult;
  }

  async #configureBuffer(
    denops: Denops,
    bufnr: number,
    prevWinid: number,
  ): Promise<void> {
    await denops.cmd("setlocal buftype=nofile");
    await denops.cmd("setlocal bufhidden=hide");
    await denops.cmd(`setlocal filetype=${RESULT_FILETYPE}`);
    await denops.cmd("setlocal noswapfile");
    await denops.cmd("setlocal nobuflisted");
    await denops.cmd("setlocal nowrap");
    await denops.cmd("setlocal nonumber norelativenumber");
    await denops.cmd("setlocal cursorline");

    await denops.call("setbufvar", bufnr, RESULT_PREV_WINID_VAR, prevWinid);

    await denops.cmd(
      `nnoremap <silent><buffer> <CR> :call denops#request('${this.#pluginName}', 'open_item', [line('.')])<CR>`,
    );
    await denops.cmd(
      `nnoremap <silent><buffer> R :call denops#request('${this.#pluginName}', 'rerun_item', [line('.')])<CR>`,
    );
    await denops.cmd(
      `nnoremap <silent><buffer> r :call denops#request('${this.#pluginName}', 'rerun_all', [])<CR>`,
    );
    await denops.cmd(
      `nnoremap <silent><buffer> q :bd!<CR>`,
    );
  }

  async #render(
    denops: Denops,
    bufnr: number,
    parsed: ParsedTestRun,
    statusLine?: string,
  ): Promise<void> {
    const results = parsed.results;
    const passed = results.filter((r) => r.result).length;
    const failed = results.length - passed;

    const summary = parsed.summary
      ? `${parsed.summary.passed} passed, ${parsed.summary.failed} failed${
        parsed.summary.durationMs != null
          ? ` (${parsed.summary.durationMs}ms)`
          : ""
      }`
      : `${passed} passed, ${failed} failed`;

    const header = statusLine ?? `denops-test-runner: ${summary}`;

    const lines: string[] = [];
    lines.push(header);
    lines.push("");

    const startLnum = 3;
    for (const r of results) {
      lines.push(formatResultLine(r));
    }

    if (results.length === 0 && !statusLine) {
      lines.push("(no test output parsed)");
    }

    await denops.cmd("setlocal modifiable");
    await setbufline(denops, bufnr, 1, lines);
    try {
      await denops.call("deletebufline", bufnr, lines.length + 1, "$");
    } catch {
      // ignore
    }
    await denops.cmd("setlocal nomodifiable");

    await denops.call("setbufvar", bufnr, RESULT_ITEMS_VAR, results);
    await denops.call("setbufvar", bufnr, RESULT_START_LNUM_VAR, startLnum);
  }
}

function parseJunit(rawOutput: string, opts: JunitParseOptions): ParsedTestRun {
  const raw = stripAnsi(rawOutput);

  const results: TestResult[] = [];

  let rootSummary: TestRunSummary | undefined;
  const rootMatch = raw.match(/<testsuites\b([^>]*)>/);
  if (rootMatch) {
    const attrs = parseXmlAttrs(rootMatch[1] ?? "");
    const tests = safeInt(attrs.tests);
    const failures = safeInt(attrs.failures);
    const errors = safeInt(attrs.errors);
    const timeSec = safeFloat(attrs.time);
    if (tests != null && failures != null && errors != null) {
      rootSummary = {
        passed: Math.max(0, tests - failures - errors),
        failed: failures + errors,
        durationMs: timeSec != null ? Math.round(timeSec * 1000) : undefined,
      };
    }
  }

  let idx = 0;
  while (true) {
    const start = raw.indexOf("<testcase", idx);
    if (start < 0) break;
    const gt = raw.indexOf(">", start);
    if (gt < 0) break;

    const head = raw.slice(start + "<testcase".length, gt);
    const attrs = parseXmlAttrs(head);

    const isSelfClosing = raw[gt - 1] === "/";
    let body = "";
    let end = gt + 1;
    if (!isSelfClosing) {
      const close = raw.indexOf("</testcase>", gt + 1);
      if (close < 0) break;
      body = raw.slice(gt + 1, close);
      end = close + "</testcase>".length;
    }

    const testname = decodeXmlEntities(attrs.name ?? "");
    const classname = decodeXmlEntities(attrs.classname ?? "");
    const line = safeInt(attrs.line);
    const col = safeInt(attrs.col);

    const failed = body.includes("<failure") || body.includes("<error");
    const skipped = body.includes("<skipped");

    const resolved = resolveJunitPath(opts, attrs, classname);
    const absPath = resolved != null
      ? toAbsolutePath(opts.rootDir, resolved)
      : "";

    const failureText = failed ? extractFailureText(body) : null;
    const errorLoc = failureText
      ? findErrorLocation(opts.rootDir, failureText)
      : null;

    const tr: TestResult = {
      framework: opts.framework,
      filename: absPath || toAbsolutePath(opts.rootDir, "UNKNOWN"),
      displayPath: absPath ? toDisplayPath(opts.rootDir, absPath) : undefined,
      classname: classname || undefined,
      testname,
      result: !failed && !(skipped && failed),
      lnum: line ?? undefined,
      col: col ?? undefined,
    };
    if (errorLoc?.filename) {
      tr.errorFile = errorLoc.filename;
      tr.errorLnum = errorLoc.lnum;
      tr.errorCol = errorLoc.col;
    }
    results.push(tr);

    idx = end;
  }

  return { results, summary: rootSummary };
}

function resolveJunitPath(
  opts: JunitParseOptions,
  attrs: Record<string, string>,
  classname: string,
): string | null {
  const fileAttr = attrs.file ? decodeXmlEntities(attrs.file) : "";
  if (fileAttr) return normalizePath(fileAttr);
  if (!classname) return null;
  const guessed = opts.resolveClassnameToPath?.(classname);
  return guessed ?? normalizePath(classname);
}

function formatResultLine(r: TestResult): string {
  const status = r.result ? "[PASS]" : "[FAIL]";
  const p = r.displayPath ?? r.filename;
  const jump = pickJumpTarget(r);
  const suffix = jump.lnum ? `:${jump.lnum}:${jump.col ?? 1}` : "";
  return `${status} ${r.testname} (${p}${suffix})`;
}

function stripAnsi(s: string): string {
  return s.replace(/\x1b\[[0-9;]*m/g, "");
}

function normalizePath(p: string): string {
  if (p.startsWith("file://")) {
    try {
      const u = new URL(p);
      return decodeURIComponent(u.pathname);
    } catch {
      return p;
    }
  }
  return p;
}

function escapeForRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function shellQuote(s: string): string {
  return `'${s.replace(/'/g, `'\\''`)}'`;
}

function guessPluginRootDir(): string {
  const here = new URL(".", import.meta.url); // .../denops/denops-test-runner/
  const root = new URL("../..", here); // .../<root>/
  return decodeURIComponent(root.pathname.replace(/\/$/, ""));
}

function toAbsolutePath(rootDir: string, p: string): string {
  if (!p) return "";
  if (p.startsWith("/")) return p;
  const trimmed = p.replace(/^\.\//, "");
  return `${rootDir}/${trimmed}`;
}

function toDisplayPath(rootDir: string, abs: string): string {
  const prefix = rootDir.endsWith("/") ? rootDir : `${rootDir}/`;
  if (abs.startsWith(prefix)) {
    return `./${abs.slice(prefix.length)}`;
  }
  return abs;
}

function parseXmlAttrs(s: string): Record<string, string> {
  const out: Record<string, string> = {};
  const re = /([A-Za-z_][A-Za-z0-9_.:-]*)="([^"]*)"/g;
  let m: RegExpExecArray | null;
  while ((m = re.exec(s)) !== null) {
    out[m[1]] = m[2];
  }
  return out;
}

function decodeXmlEntities(s: string): string {
  return s
    .replaceAll("&lt;", "<")
    .replaceAll("&gt;", ">")
    .replaceAll("&quot;", '"')
    .replaceAll("&apos;", "'")
    .replaceAll("&amp;", "&");
}

function vimString(s: string): string {
  // Single-quoted Vim script string literal.
  return `'${s.replaceAll("'", "''")}'`;
}

function pickJumpTarget(
  r: TestResult,
): { filename: string; lnum?: number; col?: number } {
  if (!r.result && r.errorFile) {
    return { filename: r.errorFile, lnum: r.errorLnum, col: r.errorCol };
  }
  return { filename: r.filename, lnum: r.lnum, col: r.col };
}

function safeInt(s: string | undefined): number | null {
  if (s == null) return null;
  const n = Number.parseInt(s, 10);
  return Number.isFinite(n) ? n : null;
}

function safeFloat(s: string | undefined): number | null {
  if (s == null) return null;
  const n = Number.parseFloat(s);
  return Number.isFinite(n) ? n : null;
}

function fileExists(path: string): boolean {
  try {
    Deno.statSync(path);
    return true;
  } catch {
    return false;
  }
}

function guessPathFromJunitClassname(
  rootDir: string,
  classname: string,
): string | null {
  const p = normalizePath(classname);
  if (p.includes("/") || p.endsWith(".ts") || p.endsWith(".js")) return p;

  // If it's a dotted module-ish name, try mapping it.
  if (p.includes(".")) {
    const cand = `${p.replaceAll(".", "/")}.ts`;
    const abs = toAbsolutePath(rootDir, cand);
    if (fileExists(abs)) return cand;
  }
  return null;
}

function resolvePytestClassname(
  rootDir: string,
  classname: string,
): string | null {
  const c = normalizePath(classname);
  if (c.includes("/") && (c.endsWith(".py") || c.endsWith(".py::"))) {
    return c.replace(/::.*$/, "");
  }
  if (c.endsWith(".py")) return c;

  // pytest junit commonly uses python dotted module/class names.
  // Try best-effort resolution to a .py file.
  const moduleLike = c.split("::")[0];
  if (moduleLike.includes(".")) {
    const cand = `${moduleLike.replaceAll(".", "/")}.py`;
    const abs = toAbsolutePath(rootDir, cand);
    if (fileExists(abs)) return cand;
  }

  // Also try treating it as a relative path without extension.
  const cand2 = `${moduleLike}.py`;
  const abs2 = toAbsolutePath(rootDir, cand2);
  if (fileExists(abs2)) return cand2;

  return null;
}

function extractFailureText(testcaseBody: string): string | null {
  const failure = extractFirstTagText(testcaseBody, "failure") ??
    extractFirstTagText(testcaseBody, "error");
  if (!failure) return null;
  const decoded = decodeXmlEntities(failure);
  return decoded.replace(/<[^>]+>/g, "");
}

function extractFirstTagText(s: string, tag: string): string | null {
  const open = s.indexOf(`<${tag}`);
  if (open < 0) return null;
  const gt = s.indexOf(">", open);
  if (gt < 0) return null;
  const close = s.indexOf(`</${tag}>`, gt + 1);
  if (close < 0) return null;
  return s.slice(gt + 1, close);
}

function findErrorLocation(
  rootDir: string,
  text: string,
): { filename: string; lnum?: number; col?: number } | null {
  // Deno stack traces commonly include:
  //   at file:///abs/path/to/file.ts:29:3
  //   at /abs/path/to/file.ts:29:3
  const denoRe =
    /(?:file:\/\/\/)?(?<file>\/[^\s):]+?\.(?:ts|tsx|js|jsx|mts|cts)):(?<lnum>\d+):(?<col>\d+)/g;
  let m: RegExpExecArray | null;
  while ((m = denoRe.exec(text)) !== null) {
    const file = normalizePath(m.groups?.file ?? "").replace(/^\/{2,}/, "/");
    const abs = toAbsolutePath(rootDir, file);
    if (abs.startsWith(rootDir)) {
      return {
        filename: abs,
        lnum: Number(m.groups!.lnum),
        col: Number(m.groups!.col),
      };
    }
  }
  denoRe.lastIndex = 0;
  m = denoRe.exec(text);
  if (m?.groups) {
    const file = normalizePath(m.groups.file).replace(/^\/{2,}/, "/");
    const abs = toAbsolutePath(rootDir, file);
    return {
      filename: abs,
      lnum: Number(m.groups.lnum),
      col: Number(m.groups.col),
    };
  }

  // Pytest tracebacks commonly include:
  //   File "/abs/path/test_x.py", line 12, in test_foo
  const pyRe = /File "(?<file>[^"]+?\.py)", line (?<lnum>\d+)/g;
  while ((m = pyRe.exec(text)) !== null) {
    const file = normalizePath(m.groups?.file ?? "");
    const abs = toAbsolutePath(rootDir, file);
    if (abs.startsWith(rootDir)) {
      return { filename: abs, lnum: Number(m.groups!.lnum), col: 1 };
    }
  }
  pyRe.lastIndex = 0;
  m = pyRe.exec(text);
  if (m?.groups) {
    const file = normalizePath(m.groups.file);
    const abs = toAbsolutePath(rootDir, file);
    return { filename: abs, lnum: Number(m.groups.lnum), col: 1 };
  }

  return null;
}

// ----------------
// Tests
// ----------------

Deno.test("FormatResultLine", () => {
  const sample: TestResult[] = [{
    framework: "deno",
    filename: "./denops/denops-test-runner/main.ts",
    testname: "ParseDenoTestOutput",
    result: true,
  }, {
    framework: "deno",
    filename: "./denops/denops-test-runner/main.ts",
    testname: "SimpleTest",
    result: false,
  }];
  const result = sample.map(formatResultLine);
  const expected = [
    "[PASS] ParseDenoTestOutput (./denops/denops-test-runner/main.ts)",
    "[FAIL] SimpleTest (./denops/denops-test-runner/main.ts)",
  ];
  assertEquals(expected, result);
});

Deno.test("ParseJunitOk", () => {
  const xml = [
    `<?xml version="1.0" encoding="UTF-8"?>`,
    `<testsuites name="deno test" tests="2" failures="0" errors="0" time="0.003">`,
    `  <testsuite name="./denops/denops-test-runner/main.ts" tests="2" disabled="0" errors="0" failures="0">`,
    `    <testcase name="ParseDenoTestOutput" classname="./denops/denops-test-runner/main.ts" time="0.000" line="10" col="6"></testcase>`,
    `    <testcase name="SimpleTest" classname="./denops/denops-test-runner/main.ts" time="0.000" line="20" col="6"></testcase>`,
    `  </testsuite>`,
    `</testsuites>`,
  ].join("\n");
  const result = parseJunit(xml, {
    rootDir: "/x/root",
    framework: "deno",
  });
  const expected: ParsedTestRun = {
    results: [{
      framework: "deno",
      filename: "/x/root/denops/denops-test-runner/main.ts",
      displayPath: "./denops/denops-test-runner/main.ts",
      classname: "./denops/denops-test-runner/main.ts",
      testname: "ParseDenoTestOutput",
      result: true,
      lnum: 10,
      col: 6,
    }, {
      framework: "deno",
      filename: "/x/root/denops/denops-test-runner/main.ts",
      displayPath: "./denops/denops-test-runner/main.ts",
      classname: "./denops/denops-test-runner/main.ts",
      testname: "SimpleTest",
      result: true,
      lnum: 20,
      col: 6,
    }],
    summary: {
      passed: 2,
      failed: 0,
      durationMs: 3,
    },
  };
  assertEquals(expected, result);
});

Deno.test("ParseJunitFailed", () => {
  const thisFile = "/x/root/denops/denops-test-runner/main.ts";
  const xml = [
    `<?xml version="1.0" encoding="UTF-8"?>`,
    `<testsuites name="deno test" tests="2" failures="1" errors="0" time="0.066">`,
    `  <testsuite name="./denops/denops-test-runner/main.ts" tests="2" disabled="0" errors="0" failures="1">`,
    `    <testcase name="SimpleTest" classname="./denops/denops-test-runner/main.ts" time="0.000" line="10" col="6"></testcase>`,
    `    <testcase name="ParseDenoTestOutput" classname="./denops/denops-test-runner/main.ts" time="0.000" line="22" col="6">`,
    `      <failure message="AssertionError">at file://${thisFile}:123:4\nstack...</failure>`,
    `    </testcase>`,
    `  </testsuite>`,
    `</testsuites>`,
  ].join("\n");
  const result = parseJunit(xml, {
    rootDir: "/x/root",
    framework: "deno",
  });
  const expected: ParsedTestRun = {
    results: [{
      framework: "deno",
      filename: "/x/root/denops/denops-test-runner/main.ts",
      displayPath: "./denops/denops-test-runner/main.ts",
      classname: "./denops/denops-test-runner/main.ts",
      testname: "SimpleTest",
      result: true,
      lnum: 10,
      col: 6,
    }, {
      framework: "deno",
      filename: "/x/root/denops/denops-test-runner/main.ts",
      displayPath: "./denops/denops-test-runner/main.ts",
      classname: "./denops/denops-test-runner/main.ts",
      testname: "ParseDenoTestOutput",
      result: false,
      lnum: 22,
      col: 6,
      errorFile: "/x/root/denops/denops-test-runner/main.ts",
      errorLnum: 123,
      errorCol: 4,
    }],
    summary: {
      passed: 1,
      failed: 1,
      durationMs: 66,
    },
  };
  assertEquals(expected, result);
});

Deno.test("SimpleTest", () => {
  const a = "test";
  assertEquals("test", a);
});
