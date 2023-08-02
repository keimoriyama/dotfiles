local status, lint = pcall(require, "lint")
if not status then
	return
end

lint.linters_by_ft = {
	lua = { "luacheck" },
	python = { "mypy" },
}

vim.api.nvim_create_autocmd({ "BufWritePre" }, {
	callback = function()
		lint.try_lint()
	end,
})
