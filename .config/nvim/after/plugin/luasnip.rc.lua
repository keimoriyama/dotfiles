local status, ls = pcall(require, "luasnip")

local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require("luasnip.extras").lambda
local rep = require("luasnip.extras").rep
local p = require("luasnip.extras").partial
local m = require("luasnip.extras").match
local n = require("luasnip.extras").nonempty
local dl = require("luasnip.extras").dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local types = require("luasnip.util.types")
local conds = require("luasnip.extras.conditions")
local conds_expand = require("luasnip.extras.conditions.expand")

if not status then
	return
end

-- Every unspecified option will be set to the default.
ls.setup({
	history = true,
	-- Update more often, :h events for more info.
	update_events = "TextChanged,TextChangedI",
	-- Snippets aren't automatically removed if their text is deleted.
	-- `delete_check_events` determines on which events (:h events) a check for
	-- deleted snippets is performed.
	-- This can be especially useful when `history` is enabled.
	delete_check_events = "TextChanged",
	ext_opts = {
		[types.choiceNode] = {
			active = {
				virt_text = { { "choiceNode", "Comment" } },
			},
		},
	},
	-- treesitter-hl has 100, use something higher (default is 200).
	ext_base_prio = 300,
	-- minimal increase in priority.
	ext_prio_increase = 1,
	enable_autosnippets = true,
	-- mapping for cutting selected text so it's usable as SELECT_DEDENT,
	-- SELECT_RAW or TM_SELECTED_TEXT (mapped via xmap).
	store_selection_keys = "<Tab>",
	-- luasnip uses this function to get the currently active filetype. This
	-- is the (rather uninteresting) default, but it's possible to use
	-- eg. treesitter for getting the current filetype by setting ft_func to
	-- require("luasnip.extras.filetype_functions").from_cursor (requires
	-- `nvim-treesitter/nvim-treesitter`). This allows correctly resolving
	-- the current filetype in eg. a markdown-code block or `vim.cmd()`.
	ft_func = function()
		return vim.split(vim.bo.filetype, ".", true)
	end,
})

-- 'recursive' dynamic snippet. Expands to some text followed by itself.
local rec_item
rec_item = function()
	return sn(
		nil,
		c(1, {
			-- Order is important, sn(...) first would cause infinite loop of expansion.
			t(""),
			sn(nil, { t({ "", "\t\\item " }), i(1), d(2, rec_item, {}) }),
		})
	)
end

local function print_table(args)
	for index, data in ipairs(args) do
		print("index:")
		print(index)

		for _, value in pairs(data) do
			print(value)
		end
	end
end

local function docstrings(args, _, old_state)
	local nodes = {
		t({ '\t"""', "" }),
		i(1, "\tA short Description"),
		t({ "" }),
	}
	local param_nodes = {}

	if old_state then
		nodes[2] = i(1, old_state.descr:get_text())
	end
	local insert = 2
	if args[1][1] ~= nil then
		for indx, arg in ipairs(vim.split(args[1][1], ",", true)) do
			if indx == 1 then
				vim.list_extend(nodes, { t({ "", "\tArguments" }) })
			end
			local inode
			-- if there was some text in this parameter, use it as static_text for this new snippet.
			if old_state and old_state[arg] then
				inode = i(insert, old_state["arg" .. arg]:get_text())
			else
				inode = i(insert)
			end
			vim.list_extend(nodes, { t({ "", "\t" }), t({ "" .. arg .. "" }), inode, t({ "" }) })
			param_nodes["arg" .. arg] = inode

			insert = insert + 1
		end
	end
	if args[2][2] ~= nil then
		local after_return = vim.split(args[2][2], " ", true)
		local exc = vim.split(after_return[6], ",", true)
		for indx, arg in pairs(exc) do
			if indx == 1 then
				vim.list_extend(nodes, { t({ "", "\tReturns" }) })
			end
			local inode
			if old_state and old_state[arg] then
				inode = i(insert, old_state["arg" .. arg]:get_text())
			else
				inode = i(insert)
			end
			vim.list_extend(nodes, { t({ "\t", "" }), t({ " " .. arg .. " " }), inode, t({ "" }) })
			param_nodes["arg" .. arg] = inode

			insert = insert + 1
		end
	end

	-- print_table(args)
	vim.list_extend(nodes, { t({ "", '\t"""', "" }) })

	param_nodes.descr = nodes[2]
	local snip = sn(nil, nodes)
	-- Error on attempting overwrite.
	snip.old_state = param_nodes
	return snip
end

ls.add_snippets("python", {
	s("def", {
		t({ "def " }),
		i(1, "name"),
		t("("),
		i(2),
		t({ "):", "" }),
		d(4, docstrings, { 2, 3 }),
		c(3, {
			sn(nil, { t({ "", "\treturn " }), i(1) }),
			t({ "" }),
		}),
	}),
}, {
	key = "python",
})

ls.add_snippets("tex", {
	-- rec_ls is self-referencing. That makes this snippet 'infinite' eg. have as many
	-- \item as necessary by utilizing a choiceNode.
	s("item", {
		t({ "\\begin{itemize}", "\t\\item " }),
		i(1),
		d(2, rec_item, {}),
		t({ "", "\\end{itemize}" }),
	}),
	s("figure", {
		t({ "\\begin{figure}[tb]", "\\centering", "\\includegraphics[scale=]{" }),
		i(1, "figure name"),
		t({ "}", "\\caption{" }),
		i(2, "caption"),
		t({ "}", "\\label{fig:" }),
		i(3, "label of figure"),
		t({ "}", "\\end{figure}" }),
	}),
	s("table", {
		t({ "\\begin{table}[htbp]", "\\centering", "\\begin{tablular}{ccccc}" }),
		i(1, "table contents"),
		t({ "\\end{tabular}", "\\caption{" }),
		i(2, "caption"),
		t({ "}", "\\label{table:" }),
		i(3, "label of table"),
		t({ "}", "\\end{table}" }),
	}),
}, {
	key = "tex",
})

vim.cmd([[imap <silent><expr> <C-k> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-k>']])
vim.cmd([[smap <silent><expr> <C-k> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-k>']])
vim.cmd([[imap <silent><expr> <C-q> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-q>']])
vim.cmd([[smap <silent><expr> <C-q> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-q>']])
