local M = {}

M.search_files = function()
	local files = vim.fs.find(function(name, _)
		return name:match(".*lua$")
	end, { limit = math.huge, type = "file" })
	local contents = vim.iter(files)
		:map(function(v)
			return read_file(v)
		end)
		:totable()
	local regex = vim.regex("local .*")
	for i, content in ipairs(contents) do
		for j, line in ipairs(content) do
			local from_idx, to_idx = regex:match_str(line["content"])
			if from_idx == nil and to_idx == nil then
				goto continue
			end
			-- print(vim.inspect(line))
			::continue::
		end
	end
	return files
end

-- bibファイルを文字列として読み込む
function read_file(path)
	local contents = {}
	local file = io.open(path, "r")
	io.input(file)
	local i = 0
	for line in io.lines() do
		-- contents[i] = line
		-- i = i + 1
		table.insert(contents, { content = line, path = path })
	end
	return contents
end

-- bibfile

-- 挿入部分のマッチを作る

return M
