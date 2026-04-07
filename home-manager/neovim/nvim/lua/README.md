# Neovim Lua Configuration Structure

This directory contains the Lua configuration for Neovim, organized into a clear, hierarchical structure for easy navigation and maintenance.

## Directory Structure

```
lua/
├── core/              # Core Neovim settings (no plugins required)
│   ├── autocmds.lua   # General autocmds
│   ├── keymaps.lua    # Key mappings
│   └── options.lua    # Neovim options and settings
├── plugins/           # Plugin configurations organized by category
│   ├── completion/    # Completion-related plugins
│   │   ├── ddc.lua           # ddc.vim configuration
│   │   ├── denippet.lua      # Snippet engine
│   │   └── insx_config.lua   # Auto-pairing
│   ├── fuzzy/         # Fuzzy finders and file navigation
│   │   ├── ddu.lua           # ddu.vim base config
│   │   ├── ddu-ui-ff.lua     # ddu fuzzy finder UI
│   │   ├── ddu-ui-filer.lua  # ddu file explorer UI
│   │   └── telescope.lua     # Telescope configuration
│   ├── git/           # Git integration
│   │   └── gin.lua           # gin.vim configuration
│   ├── init.lua       # Plugin manager initialization
│   ├── lang/          # Language-specific plugins
│   │   └── treesitter.lua    # Tree-sitter configuration
│   ├── lsp/           # LSP-related plugins
│   │   ├── lsp.lua                  # LSP base configuration
│   │   ├── lsp_signature_config.lua # Function signature help
│   │   ├── mason.lua                # LSP installer
│   │   └── none_ls.lua              # Formatting and linting
│   ├── tools/         # Utility tools
│   │   ├── csv_tools.lua      # CSV editing
│   │   ├── deol.lua           # Terminal integration
│   │   └── search_bibtex.lua  # Bibliography search
│   └── ui/            # UI enhancements
│       ├── hop_config.lua     # Motion plugin
│       └── skk.lua            # Japanese input
└── utils.lua          # Shared utility functions

```

## Loading Order

1. **Core settings** (`core/`) are loaded first from `init.lua`
2. **Utilities** (`utils.lua`) are loaded next
3. **Plugin manager** (`plugins/init.lua`) initializes and loads plugin configs
4. Most plugin configs are loaded by **dpp.vim** based on TOML specifications

## Plugin Configuration

Most plugin configurations in the `plugins/` subdirectories use special comment markers that are processed by dpp.vim:

- `lua_add {{{...}}}` - Code to run when plugin is added
- `lua_source {{{...}}}` - Code to run when plugin is sourced
- `lua_post_update {{{...}}}` - Code to run after plugin update

These configs are referenced in TOML files (`toml/*.toml`) and loaded automatically by the plugin manager at appropriate times.

## TOML Plugin Specifications

Plugin specifications are defined in `toml/` directory:
- `ddc.toml` - Completion plugins
- `ddu.toml` - Fuzzy finder plugins
- `lsp.toml` - LSP plugins
- `utils.toml` - Utility plugins
- `utils_lazy.toml` - Lazy-loaded utility plugins
- `ftplugin.toml` - Filetype-specific plugins
- `telescope.toml` - Telescope plugins

## Adding New Plugins

1. Add plugin specification to appropriate TOML file in `toml/`
2. If plugin needs Lua configuration:
   - Create a new `.lua` file in appropriate `plugins/` subdirectory
   - Reference it with `hooks_file="$BASE_DIR/lua/plugins/<category>/<name>.lua"`
3. For inline configs, use `lua_source="""..."""` in the TOML file

## Modifying Core Settings

Core Neovim settings that don't depend on plugins are in `core/`:
- Edit `core/options.lua` for Vim options
- Edit `core/keymaps.lua` for key mappings
- Edit `core/autocmds.lua` for general autocmds

## Philosophy

- **Separation of concerns**: Core settings are separate from plugin configs
- **Logical grouping**: Plugins are organized by functionality
- **Easy navigation**: Find configs quickly based on purpose
- **Scalability**: Easy to add new plugins without cluttering
