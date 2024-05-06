local M = {}

local add, later = MiniDeps.add, MiniDeps.later
function M.setup()
	later(function()
		add({ source = "L3MON4D3/LuaSnip" })
		luasnip_config()
	end)
end

function luasnip_config()
	local status, ls = pcall(require, "luasnip")
	if not status then
		return
	end
	local s = ls.snippet
	local sn = ls.snippet_node
	local isn = ls.indent_snippet_node
	local t = ls.text_node
	local i = ls.insert_node
	local f = ls.function_node
	local c = ls.choice_node
	local d = ls.dynamic_node
	local r = ls.restore_node
	local events = require("luasnip.util.events")
	local ai = require("luasnip.nodes.absolute_indexer")
	local extras = require("luasnip.extras")
	local l = extras.lambda
	local rep = extras.rep
	local p = extras.partial
	local m = extras.match
	local n = extras.nonempty
	local dl = extras.dynamic_lambda
	local fmt = require("luasnip.extras.fmt").fmt
	local fmta = require("luasnip.extras.fmt").fmta
	local conds = require("luasnip.extras.expand_conditions")
	local postfix = require("luasnip.extras.postfix").postfix
	local types = require("luasnip.util.types")
	local parse = require("luasnip.util.parser").parse_snippet
	local ms = ls.multi_snippet
	local k = require("luasnip.nodes.key_indexer").new_key

	ls.config.set_config({
		history = true,
		updateevents = "TextChanged,TextChangedI",
		enable_autosnippets = true,
		store_selection_keys = "<C-q>",
		ext_opts = {
			[types.choiceNode] = {
				active = {
					virt_text = { { "●", "GruvboxOrange" } },
				},
			},
			[types.insertNode] = {
				active = {
					virt_text = { { "●", "GruvboxBlue" } },
				},
			},
		},
	})

	ls.add_snippets("python", {
		s("ip", {
			t("import ipdb;ipdb.set_trace()"),
		}),
	})

	-- set keybinds for both INSERT and VISUAL.
	local set = vim.keymap.set
	set({ "i", "s" }, "<C-m>", "<Plug>luasnip-next-choice")
	set({ "i", "s" }, "<C-k>", "<Plug>luasnip-prev-choice")
end

return M
