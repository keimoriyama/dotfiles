import type { Entrypoint } from "jsr:@denops/std@^7.0.0";
import { assert, is } from "jsr:@core/unknownutil@^4.3.0";

export const main: Entrypoint = (denops) => {
  denops.dispatcher = {};
};
