import {
	ActionFlags,
	BaseConfig,
	Ddu,
} from "https://deno.land/x/ddu_vim@v3.7.0/types.ts";
import { ConfigArguments } from "https://deno.land/x/ddu_vim@v3.7.0/base/config.ts";
import { Denops } from "https://deno.land/x/ddu_vim@v3.7.0/deps.ts";
import { Params as FfParams } from "https://deno.land/x/ddu_ui_ff@v1.1.0/ff.ts";
import { Params as FilerParams } from "https://deno.land/x/ddu_ui_filer@v1.1.0/filer.ts";

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
					previewSplit: "horizontal",
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
					matchers: ["matcher_substring", "matcher_kensaku"],
					sorters: ["sorter_alpha"],
				},
				file_rec: {
					matchers: ["matcher_substring"],
					converters: ["converter_devicon"],
				},
				line: {
					matchers: ["matcher_kensaku"],
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
					split: "floating",
					floatingBorder: "rounded",
					floatingTitle: "filer",
					floatingTitlePos: "center",
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
						"outputs",
						"output",
						"notebooks",
					],
				},
			},
		});
		return Promise.resolve();
	}
}
