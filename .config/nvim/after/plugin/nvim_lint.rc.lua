local status, lint = pcall(require, "lint")
if not status then
	return
end

-- add
local status, mason_package = pcall(require, "mason-core.package")
if not status then
	return
end
local status, mason_registry = pcall(require, "mason-registry")
if not status then
	return
end
local sources = {}
for _, package in ipairs(mason_registry.get_installed_packages()) do
	local package_categories = package.spec.categories[1]
	if package_categories == mason_package.Cat.Linter then
		local linter_name = package.name
		for _, language in ipairs(package.spec.languages) do
			-- sources[string.lower(language)] = { linter_name }
			local lang = string.lower(language)
			local linter_by_lang = lint.linters_by_ft[lang]
			if linter_by_lang == nil then
				linter_by_lang = {}
			end
			if lint.linters[linter_name] == nil then
				error("This linter " .. linter_name .. " is not available in nvim-lint!!!")
			else
				table.insert(linter_by_lang, linter_name)
				sources[lang] = linter_by_lang
			end
		end
	end
end
lint.linters_by_ft = sources

-- print(vim.inspect(sources))
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
	callback = function()
		lint.try_lint()
	end,
})
