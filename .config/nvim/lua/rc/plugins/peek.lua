local M = {}

local add, later = MiniDeps.add, MiniDeps.later

function M.setup()
	local build = function(args)
		vim.system({ "deno", "task", "--cwd", args.path, "--quiet", "build:fast" })
	end
	later(function()
		add({
			source = "toppair/peek.nvim",
			hooks = {
				post_install = function(args)
					-- print(args.path)
					later(function()
						build(args)
					end)
				end,
				post_update = function(args)
					build(args)
				end,
			},
		})
		vim.opt.conceallevel = 0

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
