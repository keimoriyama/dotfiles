import { BaseConfig } from "https://deno.land/x/ddc_vim@v4.0.4/types.ts";
import { ConfigArguments } from "https://deno.land/x/ddc_vim@v4.0.4/base/config.ts";

export class Config extends BaseConfig {
  override async config(args: ConfigArguments): Promise<void> {
    args.contextBuilder.patchGlobal({
      ui: "pum",
      sources: [
        "denippet",
        "copilot",
        "lsp",
        "skkeleton",
        "file",
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
        ":": ["cmdline", "cmdline_history", "around"],
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
        mocword: {
          mark: "[mocword]",
          isVolatile: false,
          minAutoCompleteLength: 2,
        },
        around: { mark: "[around]" },
        file: {
          mark: "[file]",
          isVolatile: true,
          forceCompletionPattern: "S/S*",
        },
        cmdline: { mark: "[cmdline]" },
        denippet: { mark: "[denippet]" },
        buffer: { mark: "[buffer]" },
        copilot: {
          mark: "[copilot]",
          minAutoCompleteLength: 2,
        },
        lsp: {
          mark: "[lsp]",
          keywordPattern: "\\k*",
          dup: "keep",
          sorters: ["sorter_fuzzy"],
        },
        "nvim-lua": { mark: "[lua]", forceCompletionPattern: "." },
        "cmdline-history": { mark: "[cmdline-history]" },
        skkeleton: {
          mark: "[skk]",
          matchers: [],
          sorters: [],
          converters: [],
          minAutoCompleteLength: 1,
          isVolatile: true,
        },
        omni: {
          mark: "[omni]",
        },
      },
      sourceParams: {
        buffer: { requireSameFiletype: false, forceCollect: true },
        lsp: {
          enableResolveItem: true,
          enableAdditionalTextEdit: true,
          comfirmBehaviour: "replace",
        },
      },
      filterParams: {
        converter_kind_labels: {
          kindLabels: {
            Text: "",
            Method: "",
            Function: "",
            Constructor: "",
            Field: "",
            Variable: "",
            Class: "",
            Interface: "",
            Module: "",
            Property: "",
            Unit: "",
            Value: "",
            Enum: "",
            Keyword: "",
            Snippet: "",
            Color: "",
            File: "",
            Reference: "",
            Folder: "",
            EnumMember: "",
            Constant: "",
            Struct: "",
            Event: "",
            Operator: "",
            TypeParameter: "",
          },
          kindHlGroups: {
            Method: "Function",
            Function: "Function",
            Constructor: "Function",
            Field: "Identifier",
            Variable: "Identifier",
            Class: "Structure",
            Interface: "Structure",
          },
        },
      },
    });
    args.contextBuilder.patchFiletype("markdown", {
      sources: ["around", "skkeleton", "mocword"],
    });
    args.contextBuilder.patchFiletype("markdown_inline", {
      sources: ["around", "skkeleton", "mocword"],
    });
    args.contextBuilder.patchFiletype("lua", {
      sources: ["nvim-lua", "lsp", "around"],
    });

    args.contextBuilder.patchFiletype("tex", {
      sources: [
        "omni",
        "denippet",
        "around",
        "skkeleton",
        "mocword",
        "lsp",
        "buffer",
      ],
    });
    args.contextBuilder.patchFiletype("bib", {
      sources: [
        "omni",
        "denippet",
        "around",
        "skkeleton",
        "mocword",
        "lsp",
        "buffer",
      ],
    });
    for (const filetype of ["ps1", "dosbatch", "autohotkey", "registry"]) {
      args.contextBuilder.patchFiletype(filetype, {
        sources: ["around", "file"],
        sourceOptions: {
          file: {
            forceCompletionPattern: "S\\S*",
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
