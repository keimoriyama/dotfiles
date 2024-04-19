local spec = {
	{
    'hrsh7th/nvim-cmp',
    event = { 'InsertEnter', 'CmdlineEnter' },
    config = function() cmp_setup() end
  },
  { 'hrsh7th/cmp-nvim-lsp',                event = 'InsertEnter' },
  { 'hrsh7th/cmp-buffer',                  event = 'InsertEnter' },
  { 'hrsh7th/cmp-path',                    event = 'InsertEnter' },
  { 'hrsh7th/cmp-nvim-lsp-signature-help', event = 'InsertEnter' },
  { 'yutkat/cmp-mocword',                  event = 'InsertEnter' },
  { 'hrsh7th/cmp-cmdline',                 event = 'ModeChanged' },
  { 'ray-x/cmp-treesitter',                event = 'InsertEnter' },
  { 'andersevenrud/cmp-tmux',              event = 'InsertEnter' },
  { 'bydlw98/cmp-env',                     event = 'InsertEnter' },
}

function cmp_setup()
	local cmp = require("cmp")
	
	cmp.setup({
		  mapping = cmp.mapping.preset.insert({
      ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-n>"] = cmp.mapping.select_next_item(),
      ['<CR>'] = cmp.mapping.confirm({
        select = true,
        behavior = cmp.ConfirmBehavior.Insert,
      }),
      ["<S-Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
        else
          fallback()
        end
      end, { "i", "s" }),
	}),
	sources = cmp.config.sources({
      { name = 'buffer' },
      { name = "mocword" },
      { name = 'nvim_lsp' },
      { name = "path" },
      { name = "nvim_lsp_signature_help" },
      { name = "treesitter" },
      { name = 'tmux' },
      { name = 'env' },
    }),
	})
end

return spec
