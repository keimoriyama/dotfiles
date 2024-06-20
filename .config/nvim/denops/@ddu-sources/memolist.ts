import {
  BaseSource,
  DduOptions,
  Item,
  SourceOptions,
} from "https://deno.land/x/ddu_vim@v2.2.0/types.ts";
import { Denops } from "https://deno.land/x/ddu_vim@v2.2.0/deps.ts";
import { globals } from "https://deno.land/x/denops_std@v6.5.0/variable/mod.ts";
import { expand } from "https://deno.land/x/denops_std@v6.5.0/function/mod.ts";
import { walk } from "jsr:@std/fs/walk";
import { ensure, is } from "https://deno.land/x/unknownutil@v3.18.1/mod.ts";
type Params = Record<never, never>;

export class Source extends BaseSource<Params> {
  kind = "file";
  override gather(args: {
    denops: Denops;
    options: DduOptions;
    sourceOptions: SourceOptions;
    sourceParams: Params;
    input: string;
  }): ReadableStream<Item<ActionData>[]> {
    return new ReadableStream({
      async start(controller) {
        let items: Item<ActionData>[] = [];
        let path = ensure(
          await globals.get(args.denops, "memolist_path"),
          is.String,
        );
        path = ensure(await expand(args.denops, path), is.String);
        for await (const dirEntry of walk(path)) {
          const entryPath = dirEntry.path;
          const entryName = dirEntry.name;
          if (dirEntry.isDirectory) {
            continue;
          }
          items.push({
            word: entryName,
            action: {
              path: entryPath,
              isDirectory: dirEntry.isDirectory,
            },
          });
        }
        controller.enqueue(items);
        controller.close();
      },
    });
  }
  override params(): Params {
    return {};
  }
}
