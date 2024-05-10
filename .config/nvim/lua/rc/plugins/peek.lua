local M = {}

local add, later = MiniDeps.add, MiniDeps.later

function M.setup()
	later(function()
		add({
			source = "toppair/peek.nvim",
			hooks = {
				post_install = function()
					vim.fn.system("deno", "task", "--quiet", "build:fast")
				end,
				post_update = function()
					vim.fn.system("deno", "task", "--quiet", "build:fast")
				end,
			},
		})
		vim.opt.conceallevel = 0
		local status, obsidian = pcall(require, "obsidian")
		if not status then
			return
		end

		obsidian.setup({
			dir = "~/Documents/Notes/",
			completion = {
				nvim_cmp = true,
			},
			daily_notes = { folder = "daily" },
			ui = { enable = false },
		})
		vim.api.nvim_create_user_command("PeekOpen", require("peek").open, {})
		vim.api.nvim_create_user_command("PeekClose", require("peek").close, {})
		vim.api.nvim_create_user_command("Peek", function()
			if require("peek").is_open() then
				require("peek").close()
			else
				require("peek").open()
			end
		end, {})
	end)
end

return M
