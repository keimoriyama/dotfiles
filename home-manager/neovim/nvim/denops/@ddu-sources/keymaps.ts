import { DduOptions, Item, SourceOptions } from "jsr:@shougo/ddu-vim/types";
import { BaseSource } from "jsr:@shougo/ddu-vim/source";
import type { Denops } from "jsr:@denops/std";
import { type DduActionData } from "jsr:@shougo/ddu-vim/types";
import { nvim_get_keymap } from "jsr:@denops/std/function/nvim";

type Params = Record<never, never>;

export class Source extends BaseSource<Params> {
  override kind = "file";
  override gather(args: {
    denops: Denops;
    options: DduOptions;
    sourceOptions: SourceOptions;
    sourceParams: Params;
    iput: string;
  }): ReadableStream<Item<DduActionData>[]> {
    return new ReadableStream({
      async start(controller) {
        const host = args.denops.meta.host;
        const keymapItems = async () => {
          const modes = [
            // "",
            "n",
            "i",
            "c",
            "v",
            // "x",
            // "s",
            // "o",
            "t",
            // "l",
            // "ov",
            // "nv",
            // "no",
            // "os",
            // "ns",
            // "ox",
            // "nx",
            // "nox",
            // "nos",
          ];
          let items: Item<DduActionData>[] = [];
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
        // console.log(await keymapItems());
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
): Promise<Item<DduActionData>[]> {
  const keymaps = await nvim_get_keymap(denops, mode);
  const items: Item<DduActionData>[] = [];
  for (const keymap of keymaps) {
    const mapping = keymap.lhs;
    const display = keymap.mode + " " + keymap.lhs + " " + keymap.rhs + " " +
      keymap.desc;
    items.push({
      word: mapping,
      display: display,
    });
  }
  return items;
}
