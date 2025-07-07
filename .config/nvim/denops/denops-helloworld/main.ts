import type { Entrypoint } from "jsr:@denops/std@^7.0.0";
import { assert, is } from "jsr:@core/unknownutil@^4.3.0";
import * as gfm from "jsr:@deno/gfm"

export const main: Entrypoint = (denops) => {
  denops.dispatcher = {
    hello(name) {
      assert(name, is.String);
      return `Hello, ${name || "Denops"}!`;
    },
  };
};
