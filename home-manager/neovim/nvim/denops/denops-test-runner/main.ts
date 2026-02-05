import { Denops } from "jsr:@denops/std@^7.0.0";
import { system } from "jsr:@denops/std/function";
import { assertEquals } from "jsr:@std/assert@~1.0.2/equals";

import { getbufvar } from "jsr:@denops/std/function";
import { open } from "jsr:@denops/std/buffer";

import { setbufline } from "jsr:@denops/std/function";
type TestResult = {
  filename: string;
  testname: string;
  result: boolean;
};
export async function main(denops: Denops): Promise<void> {
  denops.dispatcher = {
    async test_run(): Promise<void> {
      const runner = new DenoDpsTestRunner();
      const results: TestResult[] = await runner.run_test(denops);
      const parsed_results: string[] = runner.parse_result(results);
      // const filetype = await getbufvar(denops, "%", "&filetype");
      // bufferが存在する時は、既存のbufferを上書きする
      const bufopts = await open(denops, "[TestResult]", { opener: "split" });
      await denops.cmd("setlocal buftype=nofile");
      await denops.cmd("setlocal bufhidden=hide");
      await denops.cmd("setlocal filetype=denops-test-runner");
      await denops.cmd("setlocal noswapfile");
      const bufnr = bufopts["bufnr"];
      await setbufline(denops, bufnr, 1, parsed_results);
    },
  };
  await denops.cmd(
    `command! Testrun call denops#request('${denops.name}', 'test_run', [])`,
  );
}

class BaseDpsTestRunner {
  // private bufnr: number;
  // private denops: Denops;
  // constructor(denops: Denops) {
  // this.bufnr = bufnr;
  //   this.denops = denops;
  // }

  init() {
  }
  // open buffer
  async open_result_buffer(denops: Denops, parsed_results: string[]) {
    const bufopts = await open(denops, "[TestResult]", {
      opener: "split",
    });
    await denops.cmd("setlocal buftype=nofile");
    await denops.cmd("setlocal bufhidden=hide");
    await denops.cmd("setlocal filetype=denops-test-runner");
    await denops.cmd("setlocal noswapfile");
    const bufnr = bufopts["bufnr"];
    await setbufline(denops, bufnr, 1, parsed_results);
  }
  //
  // run test
  async run_test(denops: Denops): Promise<TestResult[]> {
  }
  // parse test result
  parse_result(cli_output: string[]): TestResult[] {}
  // build test name and path
  build_test_name_and_path() {}
  // select item

  // build output
  parse_test_result(parsed_result: TestResult[]): string[] {
    let results = [];
    let template = "File: ${filename} Test: ${testname} Result: ${result}";
    for (const result of parsed_result) {
      const res = result["result"] ? "✅" : "❌";
      const output = template.replace("${filename}", result["filename"])
        .replace(
          "${testname}",
          result["testname"],
        ).replace("${result}", res);
      results.push(output);
    }
    return results;
  }

  // rerun test

  // rerun only selected test
}

class DenoDpsTestRunner extends BaseDpsTestRunner {
  override async run_test(denops: Denops): Promise<TestResult[]> {
    const raw_res = await system(
      denops,
      "deno task test",
    );
    const res = raw_res.replace(/\x1b\[[0-9;]*m/g, "");
    const res_list = res.split("\n");
    return this.parse_result(res_list);
  }

  override parse_result(cli_output: string[]): TestResult[] {
    let test_results: TestResult[] = [];
    const file_regexp = new RegExp("\.[/[a-zA-Z-\.]+]*\.ts");
    const test_regexp = new RegExp("=> [a-zA-Z]+");
    const result_regexp = new RegExp("ok|FAILED");
    for (const output of cli_output) {
      const filename_result = file_regexp.exec(output);
      const testname_result = test_regexp.exec(output);
      const result_result = result_regexp.exec(output);
      if (
        (filename_result == null) || (testname_result == null) ||
        (result_result == null)
      ) {
        continue;
      }
      const filename = filename_result[0];
      const testname = testname_result[0].replace("=> ", "");
      const result = result_result[0] == "ok" ? true : false;
      const test_result: TestResult = {
        filename: filename,
        testname: testname,
        result: result,
      };
      test_results.push(test_result);
    }
    return test_results;
  }
}

Deno.test("ParseTestResult", () => {
  const runner = new DenoDpsTestRunner();
  const sample_input: TestResult[] = [{
    filename: "./denops/denops-test-runner/main.ts",
    testname: "ParseDenoTestOutput",
    result: true,
  }, {
    filename: "./denops/denops-test-runner/main.ts",
    testname: "SimpleTest",
    result: false,
  }];
  const result = runner.parse_test_result(sample_input);
  const expected_result = [
    "File: ./denops/denops-test-runner/main.ts Test: ParseDenoTestOutput Result: ✅",
    "File: ./denops/denops-test-runner/main.ts Test: SimpleTest Result: ❌",
  ];
  assertEquals(expected_result, result);
});

Deno.test("ParseDenoTestOutput", () => {
  const runner = new DenoDpsTestRunner();
  const sample_input = [
    "./denops/denops-test-runner/main.ts => ParseDenoTestOutput ... ok (0ms)",
    "./denops/denops-test-runner/main.ts => SimpleTest ... ok (0ms)",
  ];

  const result = runner.parse_result(sample_input);
  const expected_result: TestResult[] = [{
    filename: "./denops/denops-test-runner/main.ts",
    testname: "ParseDenoTestOutput",
    result: true,
  }, {
    filename: "./denops/denops-test-runner/main.ts",
    testname: "SimpleTest",
    result: true,
  }];
  assertEquals(expected_result, result);
});

Deno.test("ParseFailedTest", () => {
  const runner = new DenoDpsTestRunner();
  const sample_input = [
    "./denops/denops-test-runner/main.ts => SimpleTest ... ok (0ms)",
    "./denops/denops-test-runner/main.ts => ParseDenoTestOutput ... FAILED (0ms)",
    "",
    " ERRORS",
    "",
    "ParseDenoTestOutput => ./denops/denops-test-runner/main.ts:22:6",
    "error: AssertionError: Values are not equal.",
    "",
    "",
    "    [Diff] Actual / Expected",
    "",
    "",
    '-   "aa"',
    "+   undefined",
    "",
    "  throw new AssertionError(message);",
    "        ^",
    "    at assertEquals (https://jsr.io/@std/assert/1.0.13/equals.ts:64:9)",
    "    at file:///Users/kei/dotfiles/.config/nvim/denops/denops-test-runner/main.ts:29:3",
    "",
    " FAILURES",
    "",
    "ParseDenoTestOutput => ./denops/denops-test-runner/main.ts:22:6",
    "",
    "FAILED | 5 passed | 1 failed (66ms)",
    "",
    "error: Test failed",
  ];
  const result = runner.parse_result(sample_input);
  const expected_result: TestResult[] = [{
    filename: "./denops/denops-test-runner/main.ts",
    testname: "SimpleTest",
    result: true,
  }, {
    filename: "./denops/denops-test-runner/main.ts",
    testname: "ParseDenoTestOutput",
    result: false,
  }];
  assertEquals(expected_result, result);
});

Deno.test("SimpleTest", () => {
  const a = "test";
  assertEquals("test", a);
});
