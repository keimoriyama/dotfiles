local spec = {
	{
		"hrsh7th/nvim-cmp",
		event = { "InsertEnter", "CmdlineEnter" },
		config = function()
			cmp_setup()
		end,
	},
	{ "hrsh7th/cmp-nvim-lsp", event = "InsertEnter" },
	{ "hrsh7th/cmp-buffer", event = "InsertEnter" },
	{ "hrsh7th/cmp-path", event = "InsertEnter" },
	{ "hrsh7th/cmp-nvim-lsp-signature-help", event = "InsertEnter" },
	{ "yutkat/cmp-mocword", event = "InsertEnter" },
	{ "hrsh7th/cmp-cmdline", event = "ModeChanged" },
	{ "ray-x/cmp-treesitter", event = "InsertEnter" },
	{ "rinx/cmp-skkeleton", event = "InsertEnter" },
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
	cmp.setup({
		performance = {
			max_view_entries = 10,
		},
		mapping = cmp.mapping.preset.insert({
			["<C-p>"] = cmp.mapping.select_prev_item(),
			["<C-n>"] = cmp.mapping.select_next_item(),
			["<CR>"] = cmp.mapping.confirm({
				select = true,
				behavior = cmp.ConfirmBehavior.Insert,
			}),
			["<S-Tab>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
				else
					fallback()
				end
			end, { "i", "s" }),
		}),
		sources = cmp.config.sources({
			{ name = "buffer" },
			{ name = "nvim_lsp" },
			{ name = "path" },
			{ name = "nvim_lsp_signature_help" },
			{ name = "treesitter" },
			-- { name = "skkeleton" },
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
