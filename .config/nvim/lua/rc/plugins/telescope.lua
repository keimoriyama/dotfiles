local spec = {{
    'nvim-telescope/telescope.nvim', tag = '0.1.6',
	lazy=false,
      dependencies = { 'nvim-lua/plenary.nvim' },
	  config = function() telescope_config() end
    }
}

function telescope_config()
	local builtin = require('telescope.builtin')
	vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
	vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
	vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
	vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
end

return spec
