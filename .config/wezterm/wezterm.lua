local wezterm = require 'wezterm'
local act = wezterm.action

wezterm.on('update-right-status', function(window, pane)
	local name = window:active_key_table()
	if name then
		name = 'TABLE: ' .. name
	end
	window:set_right_status(name or "")
end)

local keys = {
	{ key = 'l', mods = 'CMD|SHIFT', action = wezterm.action.ReloadConfiguration, },
	-- CTRL+SHIFT+Space, followed by 'r' will put us in resize-pane
	-- mode until we cancel that mode.
	{ key = 'r', mods = 'LEADER', action = act.ActivateKeyTable { name = 'resize_pane', one_shot = false, }, },
	-- CTRL+SHIFT+Space, followed by 'a' will put us in activate-pane
	-- mode until we press some other key or until 1 second (1000ms)
	-- of time elapses
	{ key = 's', mods = 'LEADER', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }, },
	{ key = 'a', mods = 'LEADER', action = act.SplitVertical { domain = 'CurrentPaneDomain' }, },
	{ key = "q", mods = "LEADER", action = act.CloseCurrentPane { confirm = false } },
	{ key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection 'Left', },
	{ key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection 'Right', },
	{ key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection 'Up', },
	{ key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection 'Down', },
	{ key = 'H', mods = 'LEADER', action = act.AdjustPaneSize { 'Left', 5 }, },
	{ key = 'J', mods = 'LEADER', action = act.AdjustPaneSize { 'Down', 5 }, },
	{ key = 'K', mods = 'LEADER', action = act.AdjustPaneSize { 'Up', 5 } },
	{ key = 'L', mods = 'LEADER', action = act.AdjustPaneSize { 'Right', 5 }, },
}

local search_key_tables = {
	-- 検索モードの時のキーバインドの設定
	search_mode = {
		{ key = 'Enter', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
		{ key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
		{ key = 'n', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
		{ key = 'p', mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
		{ key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
		{ key = 'c', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
	}
}

local hyperlink_rules = {
	-- Linkify things that look like URLs and the host has a TLD name.
	-- Compiled-in default. Used if you don't specify any hyperlink_rules.
	{
		regex = '\\b\\w+://[\\w.-]+\\.[a-z]{2,15}\\S*\\b',
		format = '$0',
	},

	-- linkify email addresses
	-- Compiled-in default. Used if you don't specify any hyperlink_rules.
	{
		regex = [[\b\w+@[\w-]+(\.[\w-]+)+\b]],
		format = 'mailto:$0',
	},

	-- file:// URI
	-- Compiled-in default. Used if you don't specify any hyperlink_rules.
	{
		regex = [[\bfile://\S*\b]],
		format = '$0',
	},

	-- Linkify things that look like URLs with numeric addresses as hosts.
	-- E.g. http://127.0.0.1:8000 for a local development server,
	-- or http://192.168.1.1 for the web interface of many routers.
	{
		regex = [[\b\w+://(?:[\d]{1,3}\.){3}[\d]{1,3}\S*\b]],
		format = '$0',
	},

	-- Make task numbers clickable
	-- The first matched regex group is captured in $1.
	{
		regex = [[\b[tT](\d+)\b]],
		format = 'https://example.com/tasks/?t=$1',
	},

	-- Make username/project paths clickable. This implies paths like the following are for GitHub.
	-- ( "nvim-treesitter/nvim-treesitter" | wbthomason/packer.nvim | wez/wezterm | "wez/wezterm.git" )
	-- As long as a full URL hyperlink regex exists above this it should not match a full URL to
	-- GitHub or GitLab / BitBucket (i.e. https://gitlab.com/user/project.git is still a whole clickable URL)
	{
		regex = [[["]?([\w\d]{1}[-\w\d]+)(/){1}([-\w\d\.]+)["]?]],
		format = 'https://www.github.com/$1/$3',
	},
}

return {
	default_prog = { '/bin/zsh', '-l' },
	font = wezterm.font 'JetBrains Mono',
	color_scheme = 'tokyonight-storm',
	leader = { key = 's', mods = 'CTRL' },
	keys = keys,
	key_tables = search_key_tables,
	hyperlink_rules = hyperlink_rules,
}
