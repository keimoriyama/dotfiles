import {
  BaseSource,
  DduOptions,
  Item,
  SourceOptions,
} from "https://deno.land/x/ddu_vim@v3.10.1/types.ts";
import { Denops } from "https://deno.land/x/ddu_vim@v3.8.1/deps.ts";
import { ActionData } from "https://deno.land/x/ddu_kind_file@v0.3.2/file.ts";
import { nvim_get_keymap } from "https://deno.land/x/denops_std@v5.2.0/function/nvim/mod.ts";

type Params = Record<never, never>;

export class Source extends BaseSource<Params> {
  kind = "ff";
  override gather(args: {
    denops: Denops;
    options: DduOptions;
    sourceOptions: SourceOptions;
    sourceParams: Params;
    iput: string;
  }): ReadableStream<Item<ActionData>[]> {
    return new ReadableStream({
      async start(controller) {
        const host = args.denops.meta.host;
        const keymapItems = async () => {
          const modes = [
            "",
            "n",
            "i",
            "c",
            "v",
            "x",
            "s",
            "o",
            "t",
            "l",
            "ov",
            "nv",
            "no",
            "os",
            "ns",
            "ox",
            "nx",
            "nox",
            "nos",
          ];
          let items: Item<ActionData>[] = [];
          if (host == "nvim") {
            for (const m of modes) {
              // コマンド毎の一覧を取得
              const keymaps = await neovim_get_keymaps(args.denops, m);
              items = items.concat(keymaps);
            }
          } // removing duplicate items
          items = items.filter(
            (value, index, self) =>
              self.findIndex((v) => v.word === value.word) === index,
          );
          return items;
        };
        controller.enqueue(await keymapItems());
        controller.close();
      },
    });
  }
  override params(): Params {
    return {};
  }
}

async function neovim_get_keymaps(
  denops: Denops,
  mode: string,
): Promise<Item<ActionData>[]> {
  const keymaps = await nvim_get_keymap(denops, mode);
  const items: Item<ActionData>[] = [];
  for (const keymap of keymaps) {
    const words = keymap.mode + " " + keymap.lhs + " " + keymap.rhs + " " +
      keymap.desc;
    items.push({
      word: words,
    });
  }
  return items;
}
