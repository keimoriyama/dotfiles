dpp_base = vim.fn.expand(os.getenv("DPP_PATH"))

vim.fn.setenv("BASE_DIR", vim.fn.expand("<sfile>:h"))
vim.fn.setenv("DPP_BASE", dpp_base)

-- set dpp runtime path
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

---ローカルプラグインの読み込み
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

vim.api.nvim_create_user_command("DppMakeState", function()
	dpp.make_state(dpp_base, config_file)
end, {})

-- vim.api.nvim_create_user_command("DppLoad", function()
-- 	dpp.load_state(dpp_base)
-- end, {})

vim.api.nvim_create_user_command("DppInstall", function()
	dpp.async_ext_action("installer", "install")
end, {})
vim.api.nvim_create_user_command("DppUpdate", function()
	dpp.async_ext_action("installer", "update")
end, {})
-- vim.api.nvim_create_user_command("DppSource", function()
-- 	dpp.source()
-- end, {})
-- vim.api.nvim_create_user_command("DppClear", function()
-- 	dpp.clear_state()
-- end, {})
-- vim.api.nvim_create_user_command("DppGet", function()
-- 	dpp.get()
-- end, {})

vim.api.nvim_create_user_command("DppGetNotInstalled", function()
	print(dpp.async_ext_action("installer", "getNotInstalled"))
end, {})
