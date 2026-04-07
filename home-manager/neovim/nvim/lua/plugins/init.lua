-- Plugin manager configuration and plugin loading

dpp_base = vim.fn.expand(os.getenv("DPP_PATH"))

vim.fn.setenv("BASE_DIR", vim.fn.expand("<sfile>:h"))
vim.fn.setenv("DPP_BASE", dpp_base)

-- Check if using dpp or lazy (dpp takes precedence)
local use_dpp = dpp_base and dpp_base ~= ""

if use_dpp then
	-- ========================================
	-- DPP Configuration
	-- ========================================
	local dpp = require("dpp")
	local config_file = vim.fn.expand("~/.config/nvim/ts/dpp_config.ts")
	
	if vim.fn["dpp#min#load_state"](dpp_base) == 1 then
		print("Dpp is not initialized")
		vim.api.nvim_create_autocmd("User", {
			pattern = "DenopsReady",
			callback = function()
				vim.notify("dpp load_state() is failed")
				dpp.make_state(dpp_base, config_file)
			end,
		})
	end

	-- Auto-reload on config changes
	vim.api.nvim_create_autocmd("BufWritePost", {
		pattern = { "*.lua", "*.vim", "*.toml", "*.ts", "vimrc", ".vimrc" },
		callback = function()
			vim.fn["dpp#check_files"]()
		end,
	})

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

	-- Load local dpp plugins
	vim.opt.rtp:append(os.getenv("BASE_DIR") .. "/denops/")
	local plugins = {
		"dpp-ext-toml",
		"dpp-ext-lazy",
		"dpp-ext-installer",
		"dpp-protocol-git",
	}
	for _, plugin in ipairs(plugins) do
		vim.opt.rtp:append(dpp_base .. "/nix_plugins/" .. plugin)
	end

	-- DPP user commands
	vim.api.nvim_create_user_command("DppMakeState", function()
		dpp.make_state(dpp_base, config_file)
	end, {})

	vim.api.nvim_create_user_command("DppInstall", function()
		dpp.async_ext_action("installer", "install")
	end, {})
	
	vim.api.nvim_create_user_command("DppUpdate", function()
		dpp.async_ext_action("installer", "update")
	end, {})

	vim.api.nvim_create_user_command("DppGetNotInstalled", function()
		print(dpp.async_ext_action("installer", "getNotInstalled"))
	end, {})
else
	-- ========================================
	-- Lazy.nvim Configuration
	-- ========================================
	local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
	if not vim.loop.fs_stat(lazypath) then
		vim.fn.system({
			"git",
			"clone",
			"--filter=blob:none",
			"https://github.com/folke/lazy.nvim.git",
			"--branch=stable",
			lazypath,
		})
	end
	vim.opt.rtp:prepend(lazypath)
	
	local status, lazy = pcall(require, "lazy")
	if not status then
		print("lazy is not installed")
		return
	end
	
	local opts = {
		root = vim.fn.expand("$HOME") .. "/.cache/lazy",
		defaults = {
			lazy = true,
		},
		change_detection = {
			enabled = false,
			notify = false,
		},
		performance = {
			cache = {
				enabled = true,
			},
		},
	}

	lazy.setup("rc.plugins", opts)
end

-- ========================================
-- Load plugin configurations
-- ========================================
-- Note: Most plugin configurations are managed by dpp.vim/lazy.nvim and loaded
-- automatically. The config files in plugins/* subdirectories contain the actual
-- configurations that will be sourced by the plugin manager at appropriate times.
--
-- Only configs that need to be loaded immediately are required here.

-- Load LSP configuration (needs to be loaded early)
local lsp_ok = pcall(require, "plugins.lsp.lsp")
if not lsp_ok then
	vim.notify("LSP config not loaded", vim.log.levels.WARN)
end
