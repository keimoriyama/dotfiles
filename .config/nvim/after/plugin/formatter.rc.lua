local status, formatter = pcall(require, "formatter")
if not status then
	return
end

formatter.setup({
	filetype = {
		lua = {
			require("formatter.filetypes.lua").stylua,
		},
		python = {
			require("formatter.filetypes.python").black,
			require("formatter.filetypes.python").isort,
		},
	},
})

vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = { "*" },
	command = "FormatWrite",
})
