local wezterm = require("wezterm")
local act = wezterm.action

wezterm.on("update-right-status", function(window, pane)
	local name = window:active_key_table()
	if name then
		name = "TABLE: " .. name
	end
	window:set_right_status(name or "")
end)

local hyperlink_rules = {
	-- Linkify things that look like URLs and the host has a TLD name.
	-- Compiled-in default. Used if you don't specify any hyperlink_rules.
	{
		regex = "\\b\\w+://[\\w.-]+\\.[a-z]{2,15}\\S*\\b",
		format = "$0",
	},

	-- linkify email addresses
	-- Compiled-in default. Used if you don't specify any hyperlink_rules.
	{
		regex = [[\b\w+@[\w-]+(\.[\w-]+)+\b]],
		format = "mailto:$0",
	},

	-- file:// URI
	-- Compiled-in default. Used if you don't specify any hyperlink_rules.
	{
		regex = [[\bfile://\S*\b]],
		format = "$0",
	},

	-- Linkify things that look like URLs with numeric addresses as hosts.
	-- E.g. http://127.0.0.1:8000 for a local development server,
	-- or http://192.168.1.1 for the web interface of many routers.
	{
		regex = [[\b\w+://(?:[\d]{1,3}\.){3}[\d]{1,3}\S*\b]],
		format = "$0",
	},

	-- Make task numbers clickable
	-- The first matched regex group is captured in $1.
	{
		regex = [[\b[tT](\d+)\b]],
		format = "https://example.com/tasks/?t=$1",
	},
}

local mouse_binding = {
	{
		event = { Down = { streak = 1, button = { WheelUp = 1 } } },
		mods = "NONE",
		action = wezterm.action.Nop,
	},

	{
		event = { Down = { streak = 1, button = { WheelDown = 1 } } },
		mods = "NONE",
		action = wezterm.action.Nop,
	},
}
return {
	font = wezterm.font_with_fallback({ "Hack Nerd Font", "HackGen35" }),
	font_size = 18,
	color_scheme = "Solarized (light) (terminal.sexy)",
	hyperlink_rules = hyperlink_rules,
	-- mouse_bindings = mouse_binding,
	hide_tab_bar_if_only_one_tab = true,
	use_ime = false,
	-- disable_default_mouse_bindings = true,
	hide_mouse_cursor_when_typing = true,
	force_reverse_video_cursor = true,
	keys = {
		{
			key = "v",
			mods = "CTRL",
			action = wezterm.action.SplitPane({
				direction = "Left",
				size = { Percent = 50 },
			}),
		},
		{
			key = "w",
			mods = "CMD",
			action = wezterm.action.CloseCurrentPane({ confirm = true }),
		},
	},
}
