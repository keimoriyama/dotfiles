# Neovim Lua Configuration Structure

This directory contains the Lua configuration for Neovim.

## Layout

```text
lua/
├── core/              # Core Neovim settings
├── plugins/           # Plugin bootstrap and module config
├── utils.lua          # Shared helpers
└── README.md
```

## Loading order

1. `core/` from `init.lua`
2. `utils.lua`
3. `plugins/init.lua`

## Plugin manager

Plugins are managed with `mini.deps`. `init.lua` bootstraps `mini.nvim`, then
loads the plugin list and the module configs in `lua/plugins/`.

## Mini.nvim modules

This config uses `mini.comment`, `mini.pairs`, `mini.surround`, `mini.ai`,
`mini.jump`, `mini.notify`, `mini.hipatterns`, `mini.git`, `mini.completion`,
`mini.pick`, `mini.extra`, `mini.snippets`, `mini.files`, and `mini.diff`.

## Plugin configs

Most plugin-specific setup lives in these files:

- `plugins/mini.lua` - mini.nvim module setup
- `plugins/completion/mini_completion.lua` - `mini.completion`
- `plugins/completion/mini_snippets.lua` - `mini.snippets`
- `plugins/fuzzy/mini_pick.lua` - `mini.pick`, `mini.extra`, `mini.files`
- `plugins/legacy.lua` - remaining plugin setup moved off dpp/lazy
- `plugins/git/gin.lua` - gin integration
- `plugins/lang/treesitter.lua` - tree-sitter setup
- `plugins/lsp/nvim_lspconfig.lua` - LSP setup
- `plugins/lsp/none_ls.lua` - formatting and linting
- `plugins/ui/skkeleton.lua` - SKK setup
- `plugins/tools/csv_tools.lua` - CSV tools
- `vim/vimtex.vim` - vimtex completion tweak

## Core settings

- `core/options.lua`
- `core/keymaps.lua`
- `core/autocmds.lua`
