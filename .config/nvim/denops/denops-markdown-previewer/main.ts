import { Denops } from "jsr:@denops/std@^7.0.0";
import { bufname, readfile } from "jsr:@denops/std/function";
import MarkdownIt from "https://esm.sh/markdown-it@14.0.0";
import { default as highlight } from "https://cdn.skypack.dev/highlight.js@11.9.0";
import { Webview } from "jsr:@webview/webview";

export async function main(denops: Denops): Promise<void> {
  denops.dispatcher = {
    async markdown_preview(): Promise<void> {
      const filename = await bufname(denops, "%");
      const content = await readfile(denops, filename);
      const content_string: string = content.join("\n");
      const html = render(content_string);
      const webview = new Webview();
      // webview.navigate(`data:text/html,${encodeURIComponent(html)}`);
      // webview.run()
    },
  };

  await denops.cmd(
    `command! Markdown call denops#request('${denops.name}', 'markdown_preview', [])`,
  );
}

// copied from https://github.com/toppair/peek.nvim/blob/master/app/src/markdownit.ts

const md = new MarkdownIt("default", {
  html: true,
  typographer: true,
  linkify: true,
  langPrefix: "language-",
  highlight: ((code, language) => {
    if (language && highlight.getLanguage(language)) {
      try {
        return highlight.highlight(code, { language }).value;
      } catch {
        return code;
      }
    }

    return "";
  }),
});
export function uniqueIdGen() {
  const count: Record<string, number> = {};
  return function (base: string) {
    base = base + "";
    if (!count[base]) count[base] = 0;
    return base + count[base]++;
  };
}

export function render(markdown: string) {
  const tokens = md.parse(markdown, {});

  tokens.forEach((token) => {
    if (token.map && token.level === 0) {
      token.attrSet("data-line-begin", String(token.map[0] + 1));
    }
  });

  return md.renderer.render(tokens, md.options, { genId: uniqueIdGen() });
}
