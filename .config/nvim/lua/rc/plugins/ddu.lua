local M = {}

local function resize()
	local lines = vim.opt.lines:get()
	local height, row = math.floor(lines * 0.7), math.floor(lines * 0.05)
	local columns = vim.opt.columns:get()
	local width, col = math.floor(columns * 0.7), math.floor(columns * 0.15)
	vim.fn["ddu#custom#patch_global"]({
		uiParams = {
			ff = {
				winCol = col,
				winRow = row,
				winWidth = width,
				winHeight = height,
				previewCol = math.floor(width),
				previewRow = row,
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

function M.setup()
	vim.fn["ddu#custom#patch_global"]({
		ui = "ff",
		sources = {
			{
				name = "file_rec",
				params = {
					ignoredDirectories = { ".git", "node_modules", "vendor", ".mypy_cache", ".hydra", 'mlruns', "logs",
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
	vim.keymap.set("n", "<leader>sr", ddu_lsp_references, opt)
end

return M
