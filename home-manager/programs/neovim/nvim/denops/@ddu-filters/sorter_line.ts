import { type DduItem } from "jsr:@shougo/ddu-vim@~6.1.0/types";
import { BaseFilter } from "jsr:@shougo/ddu-vim@~6.1.0/filter";

import type { Denops } from "jsr:@denops/core@~7.0.0";

type Params = Record<string, never>;

function f(a: DduItem, b: DduItem) {
  const pos_a = a.action.lineNr;
  const pos_b = b.action.lineNr;
  if (pos_a < pos_b) {
    return -1;
  } else if (pos_a > pos_b) {
    return 1;
  }
  return 0;
}

export class Filter extends BaseFilter<Params> {
  override filter(args: {
    denops: Denops;
    items: DduItem[];
  }): Promise<DduItem[]> {
    if (args.items.length < 1) {
      return Promise.resolve(args.items);
    }
    if (args.items[0].hasOwnProperty("action")) {
      return Promise.resolve(args.items.sort(f));
    } else {
      return Promise.resolve(args.items);
    }
  }

  override params(): Params {
    return {};
  }
}
