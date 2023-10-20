local function resize()
	local lines = vim.opt.lines:get()
	local height, row = math.floor(lines * 0.7), math.floor(lines * 0.05)
	local columns = vim.opt.columns:get()
	local width, col = math.floor(columns * 0.8), math.floor(columns * 0.1)
	vim.fn["ddu#custom#patch_global"]({
		uiParams = {
			ff = {
				winCol = col,
				winRow = row,
				winWidth = width,
				winHeight = height,
				previewCol = col,
				previewRow = row + 20,
				previewWidth = math.floor(width),
				previewHeight = height,
			},
		},
	})
end

-- rgの設定
local function ddu_rg_live()
	vim.cmd([[
	     call ddu#start(#{
            \   sources: [#{
            \     name: 'rg',
            \     options: #{
            \       matchers: [],
            \       volatile: v:true,
            \     },
            \   }],
            \   uiParams: #{
            \     ff: #{
            \       ignoreEmpty: v:false,
            \       autoResize: v:false,
            \     }
            \   },
            \ })
			]])
end

local function ddu_lsp_references()
	vim.cmd([[
	call ddu#start(#{
	    \ sync: v:true,
	    \ sources: [#{
	    \   name: 'lsp_references',
	    \ }],
	    \})
	]])
end

local function ddu_lsp_documentSymbol()
	vim.cmd([[
	call ddu#start(#{
	    \ sync: v:true,
	    \ sources: [#{
	    \   name: 'lsp_documentSymbol',
	    \ }],
	    \})
	]])
end

local function ddu_lsp_incoming_hierarchy()
	vim.cmd([[
	call ddu#start(#{
	    \ sync: v:true,
	    \ sources: [#{
	    \   name: 'lsp_callHierarchy',
	    \ }],
		\ params: #{
		\	'incomingCalls'
		\ }
	    \})
	]])
end

local function ddu_lsp_outgoing_hierarchy()
	vim.cmd([[
	call ddu#start(#{
	    \ sync: v:true,
	    \ sources: [#{
	    \   name: 'lsp_callHierarchy',
	    \ }],
		\ params: #{
		\	'outgoingCalls'
		\ }
	    \})
	]])
end

local function keymaps()
	vim.cmd([[
	call ddu#start(#{
      \   ui: 'ff',
      \   sources: [#{
	  \ 		name: 'keymaps'
      \ }],
      \})
	]])
end

local spec = {
	{
		"Shougo/ddu.vim",
		dependencies = {
			'vim-denops/denops.vim',
		},
		config = function()
			vim.fn["ddu#custom#patch_global"]({
				ui = "ff",
				sources = {
					{
						name = "file_rec",
						params = {
							ignoredDirectories = { ".git",
								"node_modules",
								"vendor",
								".mypy_cache",
								".hydra",
								'mlruns',
								".venv",
								"logs",
								'data', "outputs" },
						},
					},
				},
				sourceOptions = {
					_ = {
						matchers = { "matcher_substring" },
						sorters = { "sorter_alpha" },
					},
					file_rec = {
						matchers = { "matcher_substring" },
						converters = { "converter_devicon" },
					},
				},
				sourceParams = {
					rg = {
						args = { "--column", "--no-heading", "--color", "never" },
					},
				},
				filterParams = {
					matcher_substring = {
						highlightMatched = "Title",
					},
					matcher_fzf = {
						highlightMatched = "Search",
					},
				},
				kindOptions = {
					file = {
						defaultAction = "open",
					},
					help = {
						defaultAction = "open",
					},
					rg = {
						defaultAction = "open",
					},
					lsp = {
						defaultAction = "open",
					},
				},
				uiParams = {
					ff = {
						-- startFilter = true,
						AutoAction = {
							name = "preview",
						},
						split = "floating",
						prompt = "> ",
						floatingBorder = "rounded",
						floatingTitle = "Ddu ff",
						floatingTitlePos = "center",
						highlights = {
							floating = "Normal",
							floatingBorder = "Normal",
						},
						filterFloatingPosition = "bottom",
						previewFloating = true,
						previewSplit = "vertical",
						previewFloatingBorder = "rounded",
						previewFloatingTitle = "preview",
						previewFloatingTitlePos = "center",
					},
				},
			})

			vim.api.nvim_create_autocmd("VimResized", { callback = resize })
			-- キーマッピングの設定
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "ddu-ff",
				callback = function()
					local opt = { buffer = true, silent = true }
					vim.keymap.set("n", "<CR>", '<cmd>call ddu#ui#do_action("itemAction", {"name": "open"})<CR>', opt)
					vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#do_action("toggleSelectItem")<CR>', opt)
					vim.keymap.set("n", "i", '<cmd>call ddu#ui#do_action("openFilterWindow")<CR>', opt)
					vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
					vim.keymap.set("n", "<C-p>", '<cmd>call ddu#ui#do_action("togglePreview")<CR>', opt)
					vim.keymap.set("n", "<C-c>", '<cmd>call ddu#ui#do_action("closePreviewWindow")<CR>', opt)
				end,
			})

			vim.api.nvim_create_autocmd("FileType", {
				pattern = "ddu-ff-filter",
				callback = function()
					local opt = { buffer = true, silent = true }
					vim.keymap.set("i", "<CR>", "<esc><cmd>close<CR>", opt)
					vim.keymap.set("n", "<CR>", "<cmd>close<CR>", opt)
					vim.keymap.set("n", "q", '<cmd>close<CR><cmd>call ddu#ui#do_action("quit")<CR>', opt)
					vim.keymap.set("i", "<C-j>", '<cmd>call ddu#ui#do_action("cursorNext")', opt)
					vim.keymap.set("i", "<C-p>", '<cmd>call ddu#ui#do_action("cursorPrevious")', opt)
				end,
			})

			resize()
			local opt = { noremap = true, silent = true }
			-- ff
			vim.keymap.set("n", "<Leader>ff", "<cmd>call ddu#start()<CR>", opt)
			-- helper ff
			vim.keymap.set("n", "<leader>h", "<cmd>call ddu#start({'sources': [{'name':'help'}]})<cr>", opt)
			-- ripgrep
			vim.keymap.set("n", "<leader>fr", ddu_rg_live, opt)
			-- definition
			vim.keymap.set("n", "<leader>dr", ddu_lsp_references, opt)
			-- show reference of the values
			-- lsp related configs(maybe moving to the mason.lua?)
			vim.keymap.set("n", "<leader>sr", ddu_lsp_references, opt)
			vim.keymap.set("n", "<leader>ds", ddu_lsp_documentSymbol, opt)
			vim.keymap.set("n", "<leader>ic", ddu_lsp_incoming_hierarchy, opt)
			vim.keymap.set("n", "<leader>oc", ddu_lsp_outgoing_hierarchy, opt)

			vim.keymap.set("n", "<leader>k", keymaps, opt)

			vim.fn["ddu#custom#patch_local"]("filer", {
				ui = "filer",
				sources = {
					{ name = "file", params = {} },
				},
				sourceOptions = {
					_ = {
						columns = { "icon_filename" },
						sorters = { "sorter_alpha" },
					},
				},
				kindOptions = {
					file = {
						defaultAction = "open",
					},
				},
				uiParams = {
					filer = {
						split = "floating",
						floatingBorder = "rounded",
						floatingTitle = "filer",
						floatingTitlePos = "center",
						winRow = vim.o.lines / 2 - 15,
						winHeight = 25,
					},
				},
			})

			vim.api.nvim_create_autocmd("FileType", {
				pattern = "ddu-filer",
				callback = function()
					local opt = { buffer = true, silent = true }
					vim.keymap.set(
						"n",
						"<CR>",
						'<cmd>call ddu#ui#do_action("itemAction", {"name":"open", "params":{"command":"tabnew"}})<CR>',
						opt
					)
					vim.keymap.set("n", "<Space>", '<cmd>call ddu#ui#do_action("toggleSelectItem")<CR>', opt)
					vim.keymap.set("n", "o", '<cmd>call ddu#ui#do_action("expandItem", {"mode": "toggle"})<CR>', opt)
					vim.keymap.set("n", "q", '<cmd>call ddu#ui#do_action("quit")<CR>', opt)
					vim.keymap.set("n", "N", '<cmd>call ddu#ui#do_action("itemAction", {"name": "newFile"})<cr>', opt)
					-- vim.keymap.set("n", "N",
					-- '<cmd>call ddu#ui#multi_actions([["itemAction", {"name": "newFile"}], ["itemAction", {"name":"open", "params":{"command": "tabnew"}}])<cr>',
					-- opt)
					vim.keymap.set("n", "d", '<cmd>call ddu#ui#do_action("itemAction", {"name": "delete"})<cr>', opt)
					vim.keymap.set("n", "r", '<cmd>call ddu#ui#do_action("itemAction", {"name": "rename"})<cr>', opt)
					vim.keymap.set("n", "y", '<cmd>call ddu#ui#do_action("itemAction", {"name": "yank"})<cr>', opt)
					vim.keymap.set("n", "c", '<cmd>call ddu#ui#do_action("itemAction", {"name": "copy"})<cr>', opt)
					vim.keymap.set("n", "p", '<cmd>call ddu#ui#do_action("itemAction", {"name": "paste"})<cr>', opt)
				end,
			})

			vim.keymap.set("n", "<Leader>sf", '<cmd>call ddu#start({"name":"filer"})<cr>',
				{ noremap = true, silent = true })
		end
	},
	"Shougo/ddu-kind-file",
	"Shougo/ddu-ui-filer",
	"Shougo/ddu-ui-ff",
	"uga-rosa/ddu-source-lsp",
	"Shougo/ddu-source-file",
	"matsui54/ddu-source-help",
	"Shougo/ddu-source-file_rec",
	"Shougo/ddu-source-action",
	"shun/ddu-source-rg",
	"Shougo/ddu-column-filename",
	"ryota2357/ddu-column-icon_filename",
	"Shougo/ddu-filter-matcher_substring",
	"Shougo/ddu-filter-sorter_alpha",
	"yuki-yano/ddu-filter-fzf",
	"uga-rosa/ddu-filter-converter_devicon"
}

return spec
