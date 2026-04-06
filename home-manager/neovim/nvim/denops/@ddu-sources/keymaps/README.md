# ddu-source-keymaps

A ddu.vim source for searching and browsing Neovim keymaps with rich descriptions and metadata.

## Features

- 🔍 Search through all your Neovim keymaps
- 🌍 View both **global** and **buffer-local** keymaps
- 🎯 Filter by mode (normal, insert, visual, command, terminal)
- 📝 Display keymap descriptions when available
- 🏷️ Show keymap flags (silent, noremap, expr, nowait, etc.)
- 📋 Intelligently display description, RHS mapping, or Lua callbacks
- 🧹 Clean, formatted output with proper alignment
- 🔤 Scope indicators (G=Global, B=Buffer-local)

## Requirements

- [ddu.vim](https://github.com/Shougo/ddu.vim)
- [denops.vim](https://github.com/vim-denops/denops.vim)
- Neovim (uses `nvim_get_keymap` and `nvim_buf_get_keymap` APIs)

## Configuration

### Basic Usage

```vim
" Show all keymaps (global + buffer-local)
call ddu#start({
\   'sources': [{'name': 'keymaps'}],
\ })
```

### Parameters

#### `modes` (string[], default: `["n", "i", "c", "v", "t"]`)

Specify which modes to search for keymaps.

Available modes:
- `n`: Normal mode
- `i`: Insert mode
- `c`: Command-line mode
- `v`: Visual and Select mode
- `x`: Visual mode
- `s`: Select mode
- `o`: Operator-pending mode
- `t`: Terminal mode
- `l`: Insert, Command-line, Lang-Arg mode

Example:
```vim
call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {
\       'modes': ['n', 'v'],
\     },
\   }],
\ })
```

#### `showFlags` (boolean, default: `true`)

Display keymap flags such as `noremap`, `silent`, `expr`, `nowait`, `script`, and `buffer`.

Example:
```vim
call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {
\       'showFlags': false,
\     },
\   }],
\ })
```

#### `includeBuffer` (boolean, default: `true`)

Include buffer-local keymaps in addition to global keymaps.

Example:
```vim
" Show only global keymaps
call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {
\       'includeBuffer': false,
\     },
\   }],
\ })
```

#### `bufNr` (number, default: `0`)

Specify which buffer's local keymaps to show. `0` means the current buffer.

Example:
```vim
" Show keymaps from buffer 5
call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {
\       'bufNr': 5,
\     },
\   }],
\ })
```

## Display Format

The plugin displays keymaps in the following format:

```
<scope> <mode> <lhs>          [flags]                        → <description or rhs>
```

Example output:
```
G n  <Leader>ff       [nore,silent]                   → Find files
G n  <C-p>            [nore,silent]                   → Open file picker
B n  gd               [nore,silent]                   → Go to definition (LSP)
G i  <C-Space>        [nore,silent]                   → Trigger completion
G v  gc               [nore,silent]                   → Comment selection
```

### Field Descriptions

- **Scope**: 
  - `G` = Global keymap (defined for all buffers)
  - `B` = Buffer-local keymap (only for specific buffer/filetype)
- **Mode**: The mode where the keymap is active (e.g., `n`, `i`, `v`)
- **LHS**: The key combination that triggers the mapping
- **Flags**: Metadata about the keymap (when `showFlags` is true)
  - `nore`: Non-recursive mapping (noremap)
  - `silent`: Silent execution
  - `expr`: Expression mapping
  - `nowait`: Don't wait for more keys
  - `script`: Script-local mapping
  - `buffer`: Buffer-local mapping
- **Description/RHS**: The description (if available), RHS mapping, or `<Lua callback>` indicator

## Example Configuration

```vim
" Search all keymaps (global + buffer-local)
nnoremap <Leader>sk <Cmd>call ddu#start({
\   'sources': [{'name': 'keymaps'}],
\ })<CR>

" Search only global keymaps
nnoremap <Leader>sK <Cmd>call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {'includeBuffer': false},
\   }],
\ })<CR>

" Search only buffer-local keymaps
nnoremap <Leader>sb <Cmd>call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {
\       'modes': ['n', 'v'],
\       'includeBuffer': true,
\     },
\   }],
\ })<CR>

" Search only normal mode keymaps without flags
nnoremap <Leader>sn <Cmd>call ddu#start({
\   'sources': [{
\     'name': 'keymaps',
\     'params': {
\       'modes': ['n'],
\       'showFlags': false,
\     },
\   }],
\ })<CR>
```

## Understanding Global vs Buffer-Local Keymaps

**Global keymaps** (`G`):
- Defined for all buffers using `vim.keymap.set()` or `:map`
- Work everywhere in Neovim
- Examples: `<Leader>ff`, `<C-p>`, general navigation keys

**Buffer-local keymaps** (`B`):
- Defined only for specific buffers/filetypes
- Common in LSP configurations (e.g., `gd`, `gr`, `K`)
- Often added by filetype plugins
- Examples: markdown-specific keys, LSP bindings in code files

## Tips

- **Set descriptions for better searchability:**
  ```lua
  vim.keymap.set('n', '<Leader>ff', '<Cmd>Telescope find_files<CR>', {
    desc = 'Find files'  -- Shows up in ddu!
  })
  ```

- **Use `includeBuffer: false` to see only your global mappings:**
  ```vim
  call ddu#start({
  \   'sources': [{
  \     'name': 'keymaps',
  \     'params': {'includeBuffer': false},
  \   }],
  \ })
  ```

- **Check buffer-specific mappings** to understand what keys are available in the current filetype:
  ```vim
  " In a TypeScript file, see TypeScript/LSP specific mappings
  call ddu#start({'sources': [{'name': 'keymaps'}]})
  " Filter by 'B' to see buffer-local only
  ```

## License

MIT
