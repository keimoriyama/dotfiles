---@type LazySpec
local spec = {
	{
		"hrsh7th/nvim-cmp",
		event = { "InsertEnter", "CmdlineEnter" },
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lsp-signature-help",
			"hrsh7th/cmp-cmdline",
			"ray-x/cmp-treesitter",
			"hrsh7th/cmp-vsnip",
		},
		config = function()
			local status, cmp = pcall(require, "cmp")
			if not status then
				return
			end
			local lspkind = require("lspkind")

			local has_words_before = function()
				if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
					return false
				end
				local line, col = unpack(vim.api.nvim_win_get_cursor(0))
				return col ~= 0
					and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
			end

			cmp.setup({
				snippet = {
					expand = function(args)
						vim.fn["vsnip#anonymous"](args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					-- ["<C-p>"] = cmp.mapping.select_prev_item(),
					-- ["<C-n>"] = cmp.mapping.select_next_item(),
					-- ["<C-l>"] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.close(),
					["<C-y>"] = cmp.mapping.confirm({
						select = true,
						behavior = cmp.ConfirmBehavior.Replace,
					}),
					["<C-n>"] = vim.schedule_wrap(function(fallback)
						if cmp.visible() and has_words_before() then
							cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
						else
							fallback()
						end
					end),
					["<C-p>"] = function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						else
							fallback()
						end
					end,
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "buffer" },
					{ name = "mocword" },
					{ name = "vsnip" },
					{ name = "path" },
					{ name = "nvim_lsp_signature_help" },
					{ name = "treesitter" },
					-- { name = "omni", eyword_length = 0 },
				}),
				formatting = {
					fields = {
						"abbr",
						"kind",
						"menu",
					},
					format = lspkind.cmp_format({
						mode = "symbol_text",
						maxwidth = 50,
						ellipsis_char = "…",
						menu = {
							nvim_lsp = "[LSP]",
							nvim_lua = "[Lua]",
							path = "[Path]",
							buffer = "[Buffer]",
							emoji = "[Emoji]",
						},
					}),
				},
			})

			-- `:` cmdline setup.
			cmp.setup.cmdline(":", {
				mapping = cmp.mapping.preset.cmdline(),
				sources = cmp.config.sources({ { name = "path" } }, {
					{ name = "cmdline", option = { ignore_cmds = { "Man", "!" } } },
				}),
			})

			-- `/` cmdline setup.
			cmp.setup.cmdline("/", {
				mapping = cmp.mapping.preset.cmdline(),
				sources = { { name = "buffer" } },
			})

			vim.cmd([[highlight! default link CmpItemKind CmpItemMenuDefault]])
		end,
	},
}
return spec
