import type { Context, Item } from "@shougo/ddu-vim/types";
import { BaseSource } from "@shougo/ddu-vim/source";
import type { Denops } from "@denops/std";
import * as nvimFn from "@denops/std/function/nvim";

type Params = {
  modes: string[];
  showFlags: boolean;
  includeBuffer: boolean;
  bufNr: number;
};

type ActionData = Record<PropertyKey, never>;

export class Source extends BaseSource<Params> {
  override kind = "word";

  override gather(args: {
    denops: Denops;
    context: Context;
    sourceParams: Params;
  }): ReadableStream<Item<ActionData>[]> {
    return new ReadableStream({
      async start(controller) {
        const host = args.denops.meta.host;
        if (host !== "nvim") {
          controller.close();
          return;
        }

        const modes = args.sourceParams.modes;
        const showFlags = args.sourceParams.showFlags;
        const includeBuffer = args.sourceParams.includeBuffer;
        const bufNr = args.sourceParams.bufNr > 0
          ? args.sourceParams.bufNr
          : args.context.bufNr;
        let items: Item<ActionData>[] = [];

        for (const mode of modes) {
          // Get global keymaps
          const globalKeymaps = await neovim_get_keymaps(
            args.denops,
            mode,
            showFlags,
            "global",
          );
          items = items.concat(globalKeymaps);

          // Get buffer-local keymaps if requested
          if (includeBuffer) {
            const bufferKeymaps = await neovim_get_buffer_keymaps(
              args.denops,
              bufNr,
              mode,
              showFlags,
            );
            items = items.concat(bufferKeymaps);
          }
        }

        // Remove duplicate items based on word and display
        items = items.filter(
          (value, index, self) =>
            self.findIndex((v) =>
              v.word === value.word && v.display === value.display
            ) === index,
        );

        controller.enqueue(items);
        controller.close();
      },
    });
  }
  override params(): Params {
    return {
      modes: ["n", "i", "c", "v", "t"],
      showFlags: true,
      includeBuffer: true,
      bufNr: 0,
    };
  }
}

type KeymapInfo = {
  lhs: string;
  rhs?: string;
  mode: string;
  desc?: string;
  buffer?: number;
  nowait?: number;
  silent?: number;
  script?: number;
  expr?: number;
  noremap?: number;
  sid?: number;
  lnum?: number;
  callback?: unknown;
};

function buildFlags(keymap: KeymapInfo): string {
  const flags: string[] = [];
  if (keymap.noremap) flags.push("nore");
  if (keymap.silent) flags.push("silent");
  if (keymap.expr) flags.push("expr");
  if (keymap.nowait) flags.push("nowait");
  if (keymap.script) flags.push("script");
  if (keymap.buffer) flags.push("buffer");
  return flags.length > 0 ? `[${flags.join(",")}]` : "";
}

function formatDisplay(
  keymap: KeymapInfo,
  showFlags: boolean,
  scope: "global" | "buffer",
): string {
  const parts: string[] = [];

  // Scope indicator
  const scopeMarker = scope === "buffer" ? "B" : "G";
  parts.push(scopeMarker);

  // Mode (padded to 2 chars)
  parts.push(keymap.mode.padEnd(2));

  // LHS (key mapping) - padded to reasonable length
  parts.push(keymap.lhs.padEnd(15));

  // Flags (if enabled)
  if (showFlags) {
    const flags = buildFlags(keymap);
    if (flags) {
      parts.push(flags.padEnd(30));
    }
  }

  // RHS or description
  const rhs = keymap.rhs || "";
  const desc = keymap.desc || "";

  if (desc) {
    // If description exists, prefer it
    parts.push(`→ ${desc}`);
  } else if (rhs) {
    // Otherwise show the RHS mapping
    parts.push(`→ ${rhs}`);
  } else if (keymap.callback) {
    // Lua callback
    parts.push(`→ <Lua callback>`);
  }

  return parts.join(" ").trim();
}

async function neovim_get_keymaps(
  denops: Denops,
  mode: string,
  showFlags: boolean,
  scope: "global" | "buffer",
): Promise<Item<ActionData>[]> {
  const keymaps = await nvimFn.nvim_get_keymap(denops, mode) as KeymapInfo[];
  const items: Item<ActionData>[] = [];

  for (const keymap of keymaps) {
    const mapping = keymap.lhs;
    const display = formatDisplay(keymap, showFlags, scope);

    items.push({
      word: mapping,
      display: display,
    });
  }

  return items;
}

async function neovim_get_buffer_keymaps(
  denops: Denops,
  bufNr: number,
  mode: string,
  showFlags: boolean,
): Promise<Item<ActionData>[]> {
  const keymaps = await nvimFn.nvim_buf_get_keymap(
    denops,
    bufNr,
    mode,
  ) as KeymapInfo[];
  const items: Item<ActionData>[] = [];

  for (const keymap of keymaps) {
    const mapping = keymap.lhs;
    const display = formatDisplay(keymap, showFlags, "buffer");

    items.push({
      word: mapping,
      display: display,
    });
  }

  return items;
}
