local cache = vim.fn.expand("~/.cache")
local plugins = { 'Shougo/dpp.vim', 'denops/denops.vim' }
for _, plugin in ipairs(plugins) do
	local dir = cache .. '/dpp/repos/github.com/' .. plugin
	if vim.fn.isdirectory(dir) ~= 0 then
		vim.fn.system({
			"git",
			"clone",
			"https://github.com/" .. plugin .. ".git",
			dir,
		})
	end
	if string.find(plugin, "dpp.vim") ~= nil then
		print(dir)
		vim.opt.rtp:prepend(dir)
	end
end

vim.opt.compatible = false

-- set dpp source path
local dpp_base = "~/.cache/dpp/"
-- set dpp runtime path
local dpp_src = '~/.cache/dpp/repos/github.com/Shougo/dpp.vim'
local denops_src = '~/.cache/dpp/repos/github.com/denops/denops.vim'

-- Set dpp runtime path (required)
vim.opt.rtp:prepend(dpp_src)

if vim.fn["dpp#min#load_state"](dpp_base) then
	vim.opt.rtp:prepend(denops_src)
	vim.api.nvim_create_autocmd("User", {
		pattern = 'DenopsReady',
		callback = function(ev)
			vim.fn["dpp#make_state"](dpp_base, vim.fs.expand("./typescript/dpp_config.ts"))
		end
	})
end

vim.cmd([[
" Enable syntax highlighting
if has('syntax')
  syntax on
endif
]])
