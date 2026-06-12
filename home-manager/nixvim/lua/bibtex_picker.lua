local M = {}

M.pick = function()
  local pick = require("mini.pick")

  -- Collect .bib files: aux-detected or cwd search
  local bib_files = {}
  local aux_file = vim.fn.expand("%:r") .. ".aux"
  if vim.fn.filereadable(aux_file) == 1 then
    for line in io.lines(aux_file) do
      local bib = line:match("\\bibdata{(.+)}")
      if bib then
        for b in bib:gmatch("[^,]+") do
          local path = vim.fn.findfile(b .. ".bib", ".;")
          if path ~= "" then
            table.insert(bib_files, path)
          end
        end
      end
    end
  end
  if #bib_files == 0 then
    local found = vim.fn.systemlist("find . -maxdepth 4 -name '*.bib' 2>/dev/null")
    for _, f in ipairs(found) do
      table.insert(bib_files, f)
    end
  end
  if #bib_files == 0 then
    vim.notify("No .bib files found", vim.log.levels.WARN)
    return
  end

  -- Parse entries from all bib files
  local entries = {}
  for _, bib_path in ipairs(bib_files) do
    local content = table.concat(vim.fn.readfile(bib_path), "\n")
    for entry_type, cite_key, body in content:gmatch("@(%w+)%s*{%s*([^,]+),%s*(.-)%s*\n}") do
      if entry_type:lower() ~= "string" and entry_type:lower() ~= "preamble" then
        local title  = body:match("[Tt]itle%s*=%s*[{\"](.-)[}\"]") or ""
        local author = body:match("[Aa]uthor%s*=%s*[{\"](.-)[}\"]") or ""
        local year   = body:match("[Yy]ear%s*=%s*[{\"]*(%d%d%d%d)[}\"]*") or ""
        title = title:gsub("[{}]", "")
        author = author:gsub("[{}]", "")
        local surname = author:match("([^,\n]+)") or author
        surname = surname:match("%S+$") or surname
        local display = string.format("%-30s %s (%s) %s", cite_key, surname, year, title)
        table.insert(entries, { text = display, key = cite_key })
      end
    end
  end

  if #entries == 0 then
    vim.notify("No BibTeX entries found", vim.log.levels.WARN)
    return
  end

  pick.start({
    source = {
      name   = "BibTeX",
      items  = entries,
      choose = function(item)
        local keys = vim.api.nvim_replace_termcodes(
          string.format("a\\cite{%s}<Esc>", item.key), true, false, true
        )
        vim.api.nvim_feedkeys(keys, "n", false)
      end,
    },
  })
end

return M
