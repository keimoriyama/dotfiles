local status, lightbulb = pcall(require, "nvim-lightbulb")

if not status then
	return
end

lightbulb.setup({
	autocmd = { enabled = true },
})
