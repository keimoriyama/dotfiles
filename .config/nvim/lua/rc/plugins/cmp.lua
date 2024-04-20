local spec = {
	{
		"hrsh7th/nvim-cmp",
		event = { "InsertEnter", "CmdlineEnter" },
		config = function()
			cmp_setup()
		end,
	},
	{ "hrsh7th/cmp-nvim-lsp",                event = "InsertEnter" },
	{ "hrsh7th/cmp-buffer",                  event = "InsertEnter" },
	{ "hrsh7th/cmp-path",                    event = "InsertEnter" },
	{ "hrsh7th/cmp-nvim-lsp-signature-help", event = "InsertEnter" },
	{ "yutkat/cmp-mocword",                  event = "InsertEnter" },
	{ "hrsh7th/cmp-cmdline",                 event = "ModeChanged" },
	{ "ray-x/cmp-treesitter",                event = "InsertEnter" },
	{ "rinx/cmp-skkeleton",                  event = "insertenter" },
	{ "saadparwaiz1/cmp_luasnip",            event = "InsertEnter", dependencies = { "L3MON4D3/LuaSnip" } },
	{
		"onsails/lspkind-nvim", -- vscode-like pictograms
		config = function()
			lspkind_setup()
		end,
	},
}

function cmp_setup()
	local cmp = require("cmp")
	local lspkind = require("lspkind")
	local luasnip = require("luasnip")
	local has_words_before = function()
		if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
			return false
		end
		local line, col = unpack(vim.api.nvim_win_get_cursor(0))
		return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
	end
	cmp.setup({
		snippet = {
			expand = function(args)
				luasnip.lsp_expand(args.body)
			end,
		},
		performance = {
			max_view_entries = 10,
		},
		window = {
			completion = cmp.config.window.bordered(),
			documentation = cmp.config.window.bordered(),
		},
		mapping = cmp.mapping.preset.insert({
			["<C-p>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
				elseif luasnip.jumpable(-1) then
					luasnip.jump(-1)
				else
					fallback()
				end
			end, { "i", "s" }),
			["<C-n>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
				elseif luasnip.expand_or_jumpable() then
					luasnip.expand_or_jump()
				elseif has_words_before() then
					cmp.complete()
				else
					fallback()
				end
			end, { "i", "s" }),
			["<C-y>"] = cmp.mapping.confirm({
				select = true,
				behavior = cmp.ConfirmBehavior.Insert,
			}),
			["<CR>"] = cmp.mapping.confirm({
				select = true,
				behavior = cmp.ConfirmBehavior.Insert,
			}),
		}),
		sources = cmp.config.sources({
			{ name = "buffer" },
			{ name = "nvim_lsp" },
			{ name = "path" },
			{ name = "nvim_lsp_signature_help" },
			{ name = "treesitter" },
			{ name = "skkeleton" },
			{ name = "luasnip" },
		}),
		formatting = {
			format = lspkind.cmp_format({ with_text = false, maxwidth = 50 }),
		},
	})
	cmp.setup.filetype("markdown", {
		sources = cmp.config.sources({
			{ name = "skkeleton" },
			{ name = "buffer" },
			{ name = "mocword" },
			{ name = "buffer" },
		}),
	})
end

function lspkind_setup()
	local status, lspkind = pcall(require, "lspkind")
	if not status then
		return
	end

	lspkind.init({
		-- enables text annotations
		--
		-- default: true
		mode = "symbol",

		-- default symbol map
		-- can be either 'default' (requires nerd-fonts font) or
		-- 'codicons' for codicon preset (requires vscode-codicons font)
		--
		-- default: 'default'
		preset = "codicons",

		-- override preset symbols
		--
		-- default: {}
		symbol_map = {
			Text = "󰉿",
			Method = "󰆧",
			Function = "󰊕",
			Constructor = "",
			Field = "󰜢",
			Variable = "󰀫",
			Class = "󰠱",
			Interface = "",
			Module = "",
			Property = "󰜢",
			Unit = "󰑭",
			Value = "󰎠",
			Enum = "",
			Keyword = "󰌋",
			Snippet = "",
			Color = "󰏘",
			File = "󰈙",
			Reference = "󰈇",
			Folder = "󰉋",
			EnumMember = "",
			Constant = "󰏿",
			Struct = "󰙅",
			Event = "",
			Operator = "󰆕",
			TypeParameter = "",
			Copilot = "",
			Codeium = "",
		},
	})
end

return spec
