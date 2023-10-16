local cache = vim.fn.expand("~/.cache")
local plugins = {'Shougo/dpp.vim', 'denops/denops.vim'}
for _, plugin in ipairs(plugins) do
	local dir = cache .. '/dpp/repos/github.com/' .. plugin
	if vim.fn.isdirectory(dir) ~= 0 then
		vim.fn.system({
		"git",
		"clone",
		"https://github.com/"..plugin..".git",
		dir,
	})
	end
	if string.find(plugin, "dpp.vim")~=nil then
		vim.opt.rtp:prepend(dir) 
	end
end


--vim.cmd([[
--" Ward off unexpected things that your distro might have made, as
--" well as sanely reset options when re-sourcing .vimrc
--set nocompatible
--
--" Set dpp base path (required)
--const s:dpp_base = '~/.cache/dpp/'
--
--" Set dpp source path (required)
--const s:dpp_src = '~/.cache/dpp/repos/github.com/Shougo/dpp.vim'
--const s:denops_src = '~/.cache/dpp/repos/github.com/denops/denops.vim'
--
--" Set dpp runtime path (required)
--execute 'set runtimepath^=' .. s:dpp_src
--
--if dpp#min#load_state(s:dpp_base)
--  " NOTE: dpp#make_state() requires denops.vim
--  execute 'set runtimepath^=' .. s:denops_src
--  autocmd User DenopsReady
--  \ call dpp#make_state(s:dpp_base, '{TypeScript config file path}')
--endif
--
--" Attempt to determine the type of a file based on its name and
--" possibly its " contents. Use this to allow intelligent
--" auto-indenting " for each filetype, and for plugins that are
--" filetype specific.
--filetype indent plugin on
--
--" Enable syntax highlighting
--if has('syntax')
--  syntax on
--endif
--]])
