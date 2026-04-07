local M = {}

M.search_files = function()
	local files = vim.fs.find(function(name, _)
		return name:match(".*bib$")
	end, { limit = math.huge, type = "file" })
	local contents = vim.iter(files)
		:map(function(v)
			return read_file(v)
		end)
		:totable()
	local regex = vim.regex([[\(@.*\|title\|author\|journal\)]])
	local items = {}
	for i, content in ipairs(contents) do
		for j, line in ipairs(content) do
			local from_idx, to_idx = regex:match_str(line["text"])
			if from_idx == nil and to_idx == nil then
				goto continue
			end
			table.insert(items, line)
			::continue::
		end
	end
	return items
end

-- bibファイルを文字列として読み込む
function read_file(path)
	local contents = {}
	local file = io.open(path, "r")
	io.input(file)
	local i = 1
	for line in io.lines() do
		table.insert(contents, { text = line, path = path, lnum = i })
		i = i + 1
	end
	return contents
end

-- bibfile

-- 挿入部分のマッチを作る

return M
