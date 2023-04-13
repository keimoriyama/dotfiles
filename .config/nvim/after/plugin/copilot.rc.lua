local status, copilot = pcall(require, 'copilot')
if not status then return end

copilot.setup({
	suggestion={enabled = false},
	panel = {enabled = false}
})

local status_copilot, copilot_cmp = pcall(require, 'copilot_cmp')
if not status_copilot then return end

copilot_cmp.setup()


