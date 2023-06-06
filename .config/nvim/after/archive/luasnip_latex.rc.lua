local status, luasnip_latex = pcall(require, "luasnip-latex-snippets")
if not status then
  return
end

luasnip_latex.setup()
