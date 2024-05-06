local M = {}

local add, later , now = MiniDeps.add, MiniDeps.later, MiniDeps.now

function M.setup()
	later(add({source="nvim-lua/plenary.nvim"}))
	later(function()
		add({source="norcalli/nvim-colorizer.lua"})
			vim.opt.termguicolors = true
			require("colorizer").setup({
				"*",
			})
		end
	)
	later(function()
		add({source="ethanholz/nvim-lastplace"})
			require("nvim-lastplace").setup()
			vim.api.nvim_feedkeys("zz", "n", false)
		end
	)
	later(function()
		add({source="uga-rosa/ugaterm.nvim"})
			vim.keymap.set({ "n", "t" }, "<C-t>", "<cmd>UgatermOpen -toggle<cr>", { noremap = true, silent = true })
	end)
	later(function()
			vim.g.sonictemplate_key = 0
			vim.g.sonictemplate_intelligent_key = 0
			vim.g.sonictemplate_postfix_key = 0
			vim.g.sonictemplate_vim_template_dir = "~/.dotfiles/.config/nvim/template"
			add({source="mattn/vim-sonictemplate"})
	end)
	later(function()
		add("kana/vim-smartword")
		vim.keymap.set("n", "w", "<Plug>(smartword-w)zz")
		vim.keymap.set("n", "b", "<Plug>(smartword-b)zz")
		vim.keymap.set("n", "e", "<Plug>(smartword-e)zz")
	end)
	later(function()
		add("chrisbra/Recover.vim")
	end)
	later(function()
		add("shortcuts/no-neck-pain.nvim")
		local width = vim.fn.winwidth(0)
		if width >= 100 then
			require("no-neck-pain").enable()
		end
	end)
end

return M
