local M = {}

function M.setup()
	require("rc.plugins.utils").setup()
	require('rc.plugins.lsp').setup()
	require('rc.plugins.none-ls').setup()
end

return M
