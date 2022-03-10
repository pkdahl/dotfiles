-- vim_markdown

vim.g.vim_markdown_follow_anchor = true
vim.g.vim_markdown_folding_disabled = true
-- Highlighting for YAML front matter
vim.g.vim_markdown_frontmatter = true
vim.g.vim_markdown_new_list_item_indent = 2
vim.g.vim_markdown_no_extensions_in_markdown = true
-- Automaically save edits before following link
vim.g.vim_markdown_autowrite = true

-- nvim-compe setup should go somewhere else

require('compe').setup({
	enabled = true,
	source = {
		path = true,
		buffer = true,
		calc = true,
		nvim_lsp = true,
		nvim_lua = true,
	}
})
