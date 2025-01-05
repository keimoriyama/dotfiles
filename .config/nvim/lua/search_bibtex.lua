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
	print(vim.inspect(contents))
	return files
end

-- bibファイルを文字列として読み込む
function read_file(path)
	local contents = {}
	local file = io.open(path, "r")
	io.input(file)
	local i = 0
	for line in io.lines() do
		contents[i] = line
		i = i + 1
	end
	return contents
end

-- bibfile

-- 挿入部分のマッチを作る

return M
