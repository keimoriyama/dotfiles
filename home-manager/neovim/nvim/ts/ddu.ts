import { BaseConfig } from "jsr:@shougo/ddu-vim/config";
import { type ConfigArguments } from "jsr:@shougo/ddu-vim/config";
import { type Params as FfParams } from "jsr:@shougo/ddu-ui-ff";
import { type Params as FilerParams } from "jsr:@shougo/ddu-ui-filer";

export class Config extends BaseConfig {
  override config(args: ConfigArguments): Promise<void> {
    args.contextBuilder.patchGlobal({
      ui: "ff",
      uiParams: {
        ff: {
          autoAction: {
            name: "preview",
          },
          startAutoAction: true,
          split: "horizontal",
          // winHeight: "&lines - 8",
          // winWidth: "&columns / 2 - 2",
          winRow: 1,
          winCol: 1,
          prompt: "> ",
          floatingBorder: "rounded",
          floatingTitle: "ff",
          floatingTitlePos: "center",
          filterSplitDirection: "floating",
          highlights: {
            floating: "Normal",
            floatingBorder: "Normal",
          },
          previewFloating: false,
          // previewHeight: "&lines - 8",
          // previewWidth: "&columns / 2 - 2",
          // previewRow: 1,
          // previewCol: "&columns / 2 + 1",
          previewSplit: "horizontal",
          previewFloatingBorder: "rounded",
          previewFloatingTitle: "preview",
          previewFloatingTitlePos: "center",
        } as Partial<FfParams>,
        filer: {} as Partial<FilerParams>,
      },
      // uiOptions: {
      //   _: {
      //     filterInputFunc: "cmdline#input",
      //     filterInputOptsFunc: "cmdline#input_opts",
      //   },
      // },
      sourceOptions: {
        _: {
          matchers: ["matcher_substring", "matcher_kensaku"],
          sorters: ["sorter_alpha"],
        },
        file_rec: {
          matchers: ["matcher_substring"],
          converters: ["converter_devicon"],
        },
        line: {
          matchers: ["matcher_kensaku"],
          sorters: ["sorter_line"],
        },
        buffer: {
          sorters: ["sorter_alpha"],
        },
        git_status: {
          converters: ["converter_git_status", "converter_devicon"],
        },
        git_diff: {
          converters: [],
        },
      },
      filterParams: {
        matcher_substring: {
          highlightMatched: "Title",
        },
        matcher_fzf: {
          highlightMatched: "Search",
        },
        matcher_kensaku: {
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
        lsp_codeAction: {
          defaultAction: "apply",
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
          split: "horizontal",
          floatingBorder: "rounded",
          floatingTitle: "filer",
          floatingTitlePos: "center",
          toggle: true,
          sort: "filename",
          startAutoAction: true,
          autoAction: {
            name: "preview",
          },
        },
      },
    });

    args.contextBuilder.patchLocal("lsp", {
      sourceOptions: {
        _: {
          converters: ["converter_lsp_symbol"],
        },
      },
    });

    args.contextBuilder.patchLocal("lsp:hierarchy", {
      sourceOptions: {
        _: {
          converters: ["converter_lsp_symbol"],
        },
      },
      uiParams: {
        ff: {
          displayTree: true,
          startFilter: false,
        },
      },
    });

    args.contextBuilder.patchLocal("ff", {
      sourceParams: {
        file_rec: {
          ignoredDirectories: [
            ".git",
            "node_modules",
            "vendor",
            ".mypy_cache",
            ".hydra",
            "mlruns",
            ".venv",
            "logs",
            "data",
            "outputs",
            "output",
            "notebooks",
            ".ruff_cache",
            ".pytest_cache",
            "__pycache__",
          ],
        },
      },
    });
    return Promise.resolve();
  }
}
