 local spec = {{
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
  {
    'j-hui/fidget.nvim',
    tag = 'legacy',
    event = 'LspAttach',
    config = function()
      require 'fidget'.setup({})
    end
  },
  {
    'onsails/lspkind-nvim', -- vscode-like pictograms
    config = function() lspkind_setup() end,
  },
  }
  



function cmp_setup()
  local status, cmp = pcall(require, "cmp")
  if (not status) then return end
  local lspkind = require 'lspkind'

  local has_words_before = function()
    if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then return false end
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
  end
  cmp.setup({
    snippet = {
      expand = function(args)
      end,
    },
    mapping = cmp.mapping.preset.insert({
      ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-n>"] = cmp.mapping.select_next_item(),
      -- ['<C-l>'] = cmp.mapping.complete(),
      ["<C-l>"] = cmp.mapping {
        i = function(fallback)
        end,
      },
      ['<Esc>'] = cmp.mapping.close(),
      ['<C-{>'] = cmp.mapping.close(),
      ['<CR>'] = cmp.mapping.confirm({
        select = true,
        behavior = cmp.ConfirmBehavior.Insert,
      }),
      ["<Tab>"] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { "i", "s" }),

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
      -- { name = 'obsidian' },
      { name = 'tmux' },
      { name = 'env' },
      -- { name = 'copilot',                priority = -50 },
      -- { name = "codeium" }
    }),
    formatting = {
      format = lspkind.cmp_format({ with_text = false, maxwidth = 50 })
    },
  })

  -- `:` cmdline setup.
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      {
        name = 'cmdline',
        option = {
          ignore_cmds = { 'Man', '!' }
        }
      }
    })
  })

  -- `/` and `?` cmdline setup.
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- for copilot
  -- cmp.event:on("menu_opened", function()
  --   vim.b.copilot_suggestion_hidden = true
  -- end)
  --
  -- cmp.event:on("menu_closed", function()
  --   vim.b.copilot_suggestion_hidden = false
  -- end)

  -- for cmp + autopairs: https://github.com/windwp/nvim-autopairs#mapping-cr
  -- and it needs to come after lsp-zero is configured: https://github.com/VonHeikemen/lsp-zero.nvim/discussions/119
  local cmp_autopairs = require('nvim-autopairs.completion.cmp')
  cmp.event:on(
    'confirm_done',
    cmp_autopairs.on_confirm_done()
  )


  vim.cmd [[highlight! default link CmpItemKind CmpItemMenuDefault]]
end

function lspkind_setup()
  local status, lspkind = pcall(require, "lspkind")
  if (not status) then return end

  lspkind.init({
    -- enables text annotations
    --
    -- default: true
    mode = 'symbol',

    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    --
    -- default: {}
    symbol_map = {
      Text = "󰉿",
      Method = "󰆧",
      Function = "󰊕",
      Constructor = "",
      Field = "󰜢",
      Variable = "󰀫",
      Class = "󰠱",
      Interface = "",
      Module = "",
      Property = "󰜢",
      Unit = "󰑭",
      Value = "󰎠",
      Enum = "",
      Keyword = "󰌋",
      Snippet = "",
      Color = "󰏘",
      File = "󰈙",
      Reference = "󰈇",
      Folder = "󰉋",
      EnumMember = "",
      Constant = "󰏿",
      Struct = "󰙅",
      Event = "",
      Operator = "󰆕",
      TypeParameter = "",
      Copilot = "",
      Codeium = "",
    },
  })
end
return spec
