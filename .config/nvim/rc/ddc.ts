import { BaseConfig } from "https://deno.land/x/ddc_vim@v4.0.4/types.ts";
import { ConfigArguments } from "https://deno.land/x/ddc_vim@v4.0.4/base/config.ts";

export class Config extends BaseConfig {
  override async config(args: ConfigArguments): Promise<void> {
    args.contextBuilder.patchGlobal({
      ui: "pum",
      sources: [
        "lsp",
        "skkeleton",
        "file",
        "denippet",
        "nvim-lua",
        "buffer",
        "around",
      ],
      autoCompleteEvents: [
        "InsertEnter",
        "TextChangedI",
        "TextChangedP",
        "CmdlineEnter",
        "CmdlineChanged",
        "TextChangedT",
      ],
      cmdlineSources: {
        ":": ["cmdline", "cmdline-history", "around"],
        "/": ["around"],
        "?": ["around"],
        "=": ["input"],
      },
      sourceOptions: {
        _: {
          maxItems: 8,
          matchers: ["matcher_head", "matcher_fuzzy"],
          sorters: ["sorter_rank"],
          converters: [
            "converter_fuzzy",
            "converter_remove_overlap",
            "converter_truncate_abbr",
          ],
          minAutoCompleteLength: 1,
        },
        around: { mark: "[around]" },
        file: {
          mark: "[file]",
          isVolatile: true,
          forceCompletionPattern: "\S/\S*",
        },
        cmdline: { mark: "[cmdline]" },
        denippet: { mark: "[denippet]" },
        buffer: { mark: "[buffer]" },
        lsp: {
          mark: "[lsp]",
          keywordPattern: "\\k*",
          dup: "keep",
          sorters: ["sorter_fuzzy", "sorter_lsp-kind"],
        },
        "nvim-lua": { mark: "[lua]", forceCompletionPattern: "." },
        "cmdline-history": { mark: "[cmdline-history]" },
        skkeleton: {
          mark: "[skk]",
          matchers: ["skkeleton"],
          minAutoCompleteLength: 1,
          isVolatile: true,
        },
        obsidian: {
          mark: "[obsidian]",
          // keywordPattern: "\[\[\w*",
        },
        obsidian_new: {
          mark: "[obsidian+]",
          // keywordPattern: "\[\[\w*",
        },
      },
      sourceParams: {
        buffer: { requireSameFiletype: false, forceCollect: true },
      },
    });
    args.contextBuilder.patchFiletype("markdown", {
      sources: ["around", "skkeleton", "obsidian", "obsidian_new"],
    });
    args.contextBuilder.patchFiletype("lua", {
      sources: ["around", "nvim-lua", "lsp"],
    });
    for (const filetype of ["ps1", "dosbatch", "autohotkey", "registry"]) {
      args.contextBuilder.patchFiletype(filetype, {
        sources: ["around", "file"],
        sourceOptions: {
          file: {
            forceCompletionPattern: "\S\\\S*",
            minAutoCompleteLength: 1,
          },
        },
        sourceParams: {
          file: {
            mode: "win32",
          },
        },
      });
    }
  }
}
