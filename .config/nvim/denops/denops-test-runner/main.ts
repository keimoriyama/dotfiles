import type { Entrypoint } from "jsr:@denops/std@^7.0.0";
import { Denops } from "jsr:@denops/std@^7.0.0";
import { system } from "jsr:@denops/std/function";

export async function main(denops: Denops): Promise<void> {
  denops.dispatcher = {
    async test_run(): Promise<void> {
      const res = await system(denops, "ls");
      console.log(res);
    },
  };
  await denops.cmd(
    `command! Testrun call denops#request('${denops.name}', 'test_run', [])`,
  );
}
