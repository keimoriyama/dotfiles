import { Denops } from "jsr:@denops/std@^7.0.0";
import { bufname, readfile } from "jsr:@denops/std/function";

import { markdown2html } from "./markdown2html.ts";

export async function main(denops: Denops): Promise<void> {
  denops.dispatcher = {
    async markdown_preview(): Promise<void> {
      let filename = await bufname(denops, "%");
      let content = await readfile(denops, filename);
      let content_string: string = content.join("\n");
      let html = markdown2html(content);
      console.log(content_string);
    },
  };

  await denops.cmd(
    `command! Maze call denops#request('${denops.name}', 'markdown_preview', [])`,
  );
}
