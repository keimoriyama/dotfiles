local dpp_base = vim.fn.expand("~/.cache/dpp")

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
local dpp_src = dpp_base .. '/repos/github.com/Shougo/dpp.vim'
local denops_src = dpp_base .. '~/.cache/dpp/repos/github.com/vim-denops/denops.vim'

InitPlugin('vim-denops/denops.vim')
InitPlugin("Shougo/dpp.vim")

vim.opt.rtp:append(dpp_src)
vim.opt.rtp:append(denops_src)

if vim.fn["dpp#min#load_state"](dpp_base) == 1 then
	local plugins = {
		"Shougo/dpp.vim",
		'vim-denops/denops.vim',
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
			vim.fn["dpp#make_state"](dpp_base, "~/.config/nvim/denops/dpp_config.ts")
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


vim.cmd("filetype indent plugin on")
if vim.fn.has("syntax") then
	vim.cmd("syntax on")
end

vim.g.dps_obsidian_base_dir = "~/Documents/Notes"
vim.g.dps_obsidian_daily_note_dir = "daily"
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>nn", "<cmd>DpsObsidianToday<cr>", opts)
vim.keymap.set("n", "gf", "<cmd>DpsObsidianFollowLink<CR>", opts)

vim.api.nvim_create_user_command(
	"DppMakeState",
	function()
		vim.fn["dpp#make_state"](dpp_base, "~/.config/nvim/denops/dpp_config.ts")
	end,
	{}
)

vim.api.nvim_create_user_command(
	"DppLoad",
	function()
		vim.fn["dpp#min#load_state"](dpp_base)
	end,
	{}
)

vim.api.nvim_create_user_command(
	"DppInstall",
	function()
		vim.fn["dpp#async_ext_action"]('installer', 'install', { maxProcess = 10 })
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppUpdate",
	function()
		vim.fn["dpp#async_ext_action"]('installer', 'update', { maxProcess = 10 })
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppSource",
	function()
		vim.fn["dpp#source"]()
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppClear",
	function()
		vim.fn["dpp#clear_state"]()
	end,
	{}
)
vim.api.nvim_create_user_command(
	"DppGet",
	function()
		vim.fn['dpp#get']()
	end,
	{}
)
