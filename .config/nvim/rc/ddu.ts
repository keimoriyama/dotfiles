import {
  ActionArguments,
  ActionFlags,
  BaseConfig,
  Ddu,
} from "https://deno.land/x/ddu_vim@v3.7.0/types.ts";
import { Denops, fn } from "https://deno.land/x/ddu_vim@v3.7.0/deps.ts";
import { ConfigArguments } from "https://deno.land/x/ddu_vim@v3.7.0/base/config.ts";
import { ActionData } from "https://deno.land/x/ddu_kind_file@v0.7.1/file.ts";
import { Params as FfParams } from "https://deno.land/x/ddu_ui_ff@v1.1.0/ff.ts";
import { Params as FilerParams } from "https://deno.land/x/ddu_ui_filer@v1.1.0/filer.ts";

type Params = Record<string, unknown>;

export class Config extends BaseConfig {
  override config(args: ConfigArguments): Promise<void> {
    args.contextBuilder.patchGlobal({
      ui: "ff",
      uiParams: {
        ff: {
          AutoAction: {
            name: "preview",
          },
          split: "floating",
          prompt: "> ",
          floatingBorder: "rounded",
          floatingTitle: "Ddu ff",
          floatingTitlePos: "center",
          highlights: {
            floating: "Normal",
            floatingBorder: "Normal",
          },
          filterFloatingPosition: "bottom",
          previewFloating: true,
          previewSplit: "vertical",
          previewFloatingBorder: "rounded",
          previewFloatingTitle: "preview",
          previewFloatingTitlePos: "center",
        } as Partial<FfParams>,
        filer: {
          split: "floating",
          floatingBorder: "rounded",
          floatingTitle: "filer",
          floatingTitlePos: "center",
          toggle: true,
          sort: "filename",
        } as Partial<FilerParams>,
      },
      sourceOptions: {
        _: {
          matchers: ["matcher_substring"],
          sorters: ["sorter_alpha"],
        },
        file_rec: {
          matchers: ["matcher_substring"],
          converters: ["converter_devicon"],
        },
      },
      sourceParams: {
        rg: {
          args: ["--column", "--no-heading", "--color", "never"],
        },
      },
      filterParams: {
        matcher_substring: {
          highlightMatched: "Title",
        },
        matcher_fzf: {
          highlightMatched: "Search",
        },
      },
      kindOptions: {
        file: {
          defaultAction: "open",
        },
        help: {
          defaultAction: "open",
        },
        rg: {
          defaultAction: "open",
        },
        lsp: {
          defaultAction: "open",
        },
      },
    });
    args.contextBuilder.patchLocal("filer", {
      ui: "filer",
      sources: {
        name: "file",
        params: {},
      },
      sourceOptions: {
        _: {
          columns: ["icon_filename"],
          sorters: ["sorter_alpha"],
        },
      },
      kindOptions: {
        file: {
          defaultAction: "open",
        },
      },
      uiParams: {
        filer: {
          split: "floating",
          floatingBorder: "rounded",
          floatingTitle: "filer",
          floatingTitlePos: "center",
        },
      },
    });
    args.contextBuilder.patchLocal("file_rec", {
      sources: {
        name: "file_rec",
        params: {
          ignoreDirectories: [
            ".git",
            "node_modules",
            "vendor",
            ".mypy_cache",
            ".hydra",
            "mlruns",
            ".venv",
            "logs",
            "data",
          ],
        },
      },
    });
    return Promise.resolve();
  }
}
