local M = {}

local add, later = MiniDeps.add, MiniDeps.later

function M.setup()
	later(function()
		add({
			source = "toppair/peek.nvim",
			hooks = {
				post_install = function()
					vim.cmd("!deno task --quiet build:fast")
				end,
				post_update = function()
					vim.cmd("!deno task --quiet build:fast")
				end,
			},
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
