-- lua_source{{{
function split(str, ts)
	-- 引数がないときは空tableを返す
	if ts == nil then
		return {}
	end
	local t = {}
	local i = 1
	for s in string.gmatch(str, "([^" .. ts .. "]+)") do
		t[i] = s
		i = i + 1
	end

	return t
end

function get_prj_name()
	local prj_dirs = vim.fs.find(".git", { type = "directory", upward = true })
	local prj_name = "empty_project"
	for _, v in ipairs(prj_dirs) do
		local names = split(v, "/")
		prj_name = names[#names - 1]
		return prj_name
	end
	return prj_name
end

local no_neck = require("no-neck-pain")

no_neck.setup({
	buffers = {
		scratchPad = {
			enabled = true,
			pathToFile = string.format("~/Documents/notes/%s.md", get_prj_name()),
		},
		bo = {
			filetype = "md",
		},
		right = {
			enabled = false,
		},
	},
})
no_neck.enable()
-- }}}
