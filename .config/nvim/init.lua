local CACHE = vim.fn.expand('~/.cache')
if vim.fn.isdirectory(CACHE) == 0 then
  vim.fn.mkdir(CACHE, 'p')
end

local plugins = {
  'Shougo/dpp.vim',
  'denops/denops.vim',
}

for _, plugin in pairs(plugins) do
  if not vim.fn.has('syntax') then
    return
  end

  local repo_name = vim.fn.fnamemodify(plugin, ':t')
  local repo_path = repo_name
  if not vim.fn.globpath('&runtimepath', repo_name, 1) then
    -- Search from the current directory
    repo_path = vim.fn.fnamemodify(repo_name, ':p')
    if vim.fn.isdirectory(repo_path) == 0 then
      -- Search from $CACHE directory
      repo_path = CACHE .. '/dpp/repos/github.com/' .. plugin
      if vim.fn.isdirectory(repo_path) == 0 then
        vim.fn.system('git clone https://github.com/' .. plugin .. ' ' .. repo_path)
      end
    end
  end

  if repo_name == 'dpp.vim' then
    vim.o.runtimepath = repo_path .. ',' .. vim.o.runtimepath
  end
end

