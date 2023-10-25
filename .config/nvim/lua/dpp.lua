local cache = vim.fn.expand("~/.cache/dpp")
local function InitPlugin(plugin)
	local dir = cache .. '/repos/github.com/' .. plugin
	if vim.fn.isdirectory(dir) ~= 0 then
		vim.fn.system({
			"git",
			"clone",
			"https://github.com/" .. plugin .. ".git",
			dir,
		})
		vim.opt.rtp:prepend(dir)
	end
end

vim.opt.compatible = false

-- set dpp source path
local dpp_base = vim.fn.expand("~/.cache/dpp/")
-- set dpp runtime path
local dpp_src = vim.fn.expand('~/.cache/dpp/repos/github.com/Shougo/dpp.vim')
local denops_src = vim.fn.expand('~/.cache/dpp/repos/github.com/denops/denops.vim')
local plugins = {
	"Shougo/dpp.vim",
	'Shougo/dpp-ext-toml',
	"Shougo/dpp-ext-lazy",
	'Shougo/dpp-ext-installer',
	"Shougo/dpp-ext-lazy",
	'Shougo/dpp-protocol-git',
	'denops/denops.vim' }
for _, plugin in ipairs(plugins) do
	InitPlugin(plugin)
end
-- Set dpp runtime path (required)
vim.opt.rtp:prepend(dpp_src)
if vim.fn["dpp#min#load_state"](dpp_base) then
	vim.opt.rtp:prepend(denops_src)
	vim.api.nvim_create_autocmd("User", {
		pattern = 'DenopsReady',
		callback = function(ev)
			vim.fn["dpp#make_state"](dpp_base, vim.fn.getcwd() .. "/denops/dpp_config.ts")
		end
	})
end

vim.cmd([[
" Enable syntax highlighting
if has('syntax')
  syntax on
endif
]])
