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

local status, filetypes = pcall(require, "formatter.filetypes")
if not status then
	return
end

local status, mason_package = pcall(require, "mason-core.package")
if not status then
	return
end
local status, mason_registry = pcall(require, "mason-registry")
if not status then
	return
end

-- for _, package in ipairs(mason_registry.get_installed_packages()) do
-- 	local package_categories = package.spec.categories[1]
-- 	if package_categories == mason_package.Cat.Formatter then
-- 		print(vim.inspect(package.name))
-- 	end
-- end

print(vim.inspect(filetypes["python"]["black"]))
vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = { "*" },
	command = "FormatWrite",
})
