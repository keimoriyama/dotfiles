local function has_plug_mapping(name, mode)
	return vim.fn.maparg(name, mode or "n") ~= ""
end

if has_plug_mapping("<Plug>(gf-improved-gf)") then
	vim.keymap.set({ "n", "x" }, "<leader>gf", "<Plug>(gf-improved-gf)", { remap = true })
	vim.keymap.set({ "n", "x" }, "<leader>gF", "<Plug>(gf-improved-gF)", { remap = true })
end

if has_plug_mapping("<Plug>(dmacro-play-macro)") then
	vim.keymap.set({ "i", "n" }, "<C-d>", "<Plug>(dmacro-play-macro)<CR>", { remap = true })
end

local ok_dial, dial_map = pcall(require, "dial.map")
if ok_dial then
	vim.keymap.set("n", "<C-a>", function()
		dial_map.manipulate("increment", "normal")
	end)
	vim.keymap.set("n", "<C-x>", function()
		dial_map.manipulate("decrement", "normal")
	end)
	vim.keymap.set("n", "g<C-a>", function()
		dial_map.manipulate("increment", "gnormal")
	end)
	vim.keymap.set("n", "g<C-x>", function()
		dial_map.manipulate("decrement", "gnormal")
	end)
	vim.keymap.set("x", "<C-a>", function()
		dial_map.manipulate("increment", "visual")
	end)
	vim.keymap.set("x", "<C-x>", function()
		dial_map.manipulate("decrement", "visual")
	end)
	vim.keymap.set("x", "g<C-a>", function()
		dial_map.manipulate("increment", "gvisual")
	end)
	vim.keymap.set("x", "g<C-x>", function()
		dial_map.manipulate("decrement", "gvisual")
	end)
end

if has_plug_mapping("<Plug>(smartword-w)") then
	vim.keymap.set("n", "w", "<Plug>(smartword-w)", { remap = true })
	vim.keymap.set("n", "b", "<Plug>(smartword-b)", { remap = true })
	vim.keymap.set("n", "e", "<Plug>(smartword-e)", { remap = true })
end

local ok_skel = pcall(require, "skkeleton")
if ok_skel then
	vim.keymap.set({ "i", "c" }, "<C-j>", "<Plug>(skkeleton-enable)", { remap = true })
	vim.keymap.set({ "i", "c" }, "<C-l>", "<Plug>(skkeleton-disable)", { remap = true })
end
