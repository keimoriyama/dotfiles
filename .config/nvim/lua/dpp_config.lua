local dpp_base = vim.fn.expand(os.getenv("DPP_PATH"))

vim.fn.setenv("BASE_DIR", vim.fn.expand("<sfile>:h"))

local function InitPlugin(plugin)
	local dir = dpp_base .. '/repos/github.com/' .. plugin
	if vim.fn.isdirectory(dir) == 0 then
		vim.fn.system({
			"git",
			"clone",
			"https://github.com/" .. plugin .. ".git",
			dir,
		})
	end
	vim.opt.rtp:append(dir)
end

vim.opt.compatible = false

-- set dpp runtime path

InitPlugin('vim-denops/denops.vim')
InitPlugin("Shougo/dpp.vim")

local dpp = require("dpp")
local config_file = vim.fn.expand("~/.config/nvim/rc/dpp_config.ts")

if vim.fn['dpp#min#load_state'](dpp_base) == 1 then
	local plugins = {
		'Shougo/dpp-ext-toml',
		"Shougo/dpp-ext-lazy",
		'Shougo/dpp-ext-installer',
		'Shougo/dpp-protocol-git',
		"Shougo/dpp-ext-local",
	}
	for _, plugin in ipairs(plugins) do
		InitPlugin(plugin)
	end
	vim.api.nvim_create_autocmd("User", {
		pattern = 'DenopsReady',
		callback = function()
			vim.notify("dpp load_state() is failed")
			dpp.make_state(dpp_base, config_file)
		end
	})
else
	vim.api.nvim_create_autocmd(
		"BufWritePost", {
			pattern = { "*.lua", "*.vim", "*.toml", "*.ts", "vimrc", ".vimrc" },
			callback = function()
				vim.fn["dpp#check_files"]()
			end
		}
	)
end

vim.api.nvim_create_autocmd("User", {
	pattern = "Dpp:makeStatePost",
	callback = function()
		vim.notify("dpp make_state() is done")
	end,
})

vim.cmd("filetype indent plugin on")
if vim.fn.has("syntax") then
	vim.cmd("syntax on")
end

vim.opt.path:append(dpp_base .. "repos/github.com/high-moctane/mocword-data/")

vim.g.dps_obsidian_base_dir = "~/Documents/Notes"
vim.g.dps_obsidian_daily_note_dir = "daily"
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>nn", "<cmd>DpsObsidianToday<cr>", opts)
vim.keymap.set("n", "gf", "<cmd>DpsObsidianFollowLink<CR>", opts)

vim.api.nvim_create_user_command(
	"DppMakeState",
	function()
		dpp.make_state(dpp_base, config_file)
	end,
	{}
)

vim.api.nvim_create_user_command(
	"DppLoad",
	function()
		dpp.load_state(dpp_base)
	end,
	{}
)

vim.api.nvim_create_user_command(
	"DppInstall",
	function()
		dpp.async_ext_action('installer', 'install')
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppUpdate",
	function()
		dpp.async_ext_action('installer', 'update')
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppSource",
	function()
		dpp.source()
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppClear",
	function()
		dpp.clear_state()
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppGet",
	function()
		dpp.get()
	end,
	{}
)

vim.api.nvim_create_user_command(
	"DppGetNotInstalled",
	function()
		print(dpp.async_ext_action('installer', 'getNotInstalled'))
	end,
	{}
)
