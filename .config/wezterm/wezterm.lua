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
	{
		key = 'l',
		mods = 'CMD|SHIFT',
		action = wezterm.action.ReloadConfiguration,
	},
	-- CTRL+SHIFT+Space, followed by 'r' will put us in resize-pane
	-- mode until we cancel that mode.
	{
		key = 'r',
		mods = 'LEADER',
		action = act.ActivateKeyTable {
			name = 'resize_pane',
			one_shot = false,
		},
	},
	-- CTRL+SHIFT+Space, followed by 'a' will put us in activate-pane
	-- mode until we press some other key or until 1 second (1000ms)
	-- of time elapses
	{
		key = 'a',
		mods = 'LEADER',
		action = act.ActivateKeyTable {
			name = 'activate_pane',
			timeout_milliseconds = 1000,
		},
	},
	{
		key = 's',
		mods = 'LEADER',
		action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
	},
	{
		key = 'a',
		mods = 'LEADER',
		action = act.SplitVertical { domain = 'CurrentPaneDomain' },
	},
	{
		key = "q",
		mods = "LEADER",
		action = wezterm.action.CloseCurrentPane { confirm = false }
	},
}

return {
	font = wezterm.font 'JetBrains Mono',
	color_scheme = 'tokyonight-storm',
	leader = { key = 's', mods = 'CTRL' },
	keys = keys,
}
