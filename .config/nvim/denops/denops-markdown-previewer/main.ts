// import type { Entrypoint } from "jsr:@denops/std@^7.0.0";
// import { assert, is } from "jsr:@core/unknownutil@^4.3.0";
import { Denops } from "jsr:@denops/std@^7.0.0";
import { bufname } from "jsr:@denops/std@^7.0.0/function";

import { markdown2html } from "./markdown2html.ts";

export async function main(denops: Denops): Promise<void> {
  denops.dispatcher = {
    async markdown_preview(): Promise<void> {
      console.log(bufname("%"));
    },
  };

  await denops.cmd(
    `command! Maze call denops#request('${denops.name}', 'markdown_preview', [])`,
  );
}
