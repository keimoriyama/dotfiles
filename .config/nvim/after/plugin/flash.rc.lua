local status, flash = pcall(require, "flash")
if not status then
	return
end

flash.setup({
	labels = "asdfghjklqwertyuiopzxcvbnm",
	char = {
		jump_labels = true,
	},
})
