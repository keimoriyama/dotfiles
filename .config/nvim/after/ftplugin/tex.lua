local status, cmp = pcall(require, "cmp")
if not status then return end

cmp.setup.buffer {
    formatting = {
        format = function(entry, vim_item)
            vim_item.menu = ({
                omni = (vim.inspect(vim_item.menu):gsub('%"', "")),
                buffer = "[Buffer]"
                -- formatting for other sources
            })[entry.source.name]
            return vim_item
        end
    },
    sources = {
        {name = 'omni', keyword_length = 0}, {name = "nvim_lsp"},
        {name = "buffer"}, {name = "mocword"}, {name = "path"},
        {name = "nvim_lsp_signature_help"}, {name = "treesitter"},
        {name = "copilot"}, {name = 'latex_symbols', option = {strategy = 0}}
        -- other sources
    }
}
