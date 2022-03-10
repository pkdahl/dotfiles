local map = vim.api.nvim_set_keymap
local mopts = {noremap = true}
-- local which_key_leader = {name = '+Leader'}

-- map('n', '<Space>', '', {})
-- map('n', '<BS>', '<Leader>', {noremap = false})
-- vim.api.nvim_set_var('mapleader', ' ')
vim.api.nvim_set_var("mapleader", ",")

map('i', 'jk', '<Esc>', {noremap = false})
map('n', '<C-l>', '<Cmd>nohlsearch<CR>', mopts)
map("n", "Y", "y$", {noremap = true})


-- Buffer mappings

require('which-key').register(
    {
        b = {
            name = "buffers",
            b = {
                [[<Cmd>lua require('telescope.builtin').buffers({sort_mru = true})<CR>]],
                "Open buffer"
            },
            d = {
                [[<Cmd>bprev | bdelete #<CR>]],
                "Delete buffer"
            },
        },
        f = {
            name = "files",
            b = {
                [[<Cmd>lua require('telescope.builtin').file_browser()<CR>]],
                "Browse files",
            },
            f = {
                [[<Cmd>lua require('telescope.builtin').find_files({theme = get_dropdown})<CR>]],
                "Open file",
            },
            r = {
                [[<Cmd>lua require('telescope.builtin').oldfiles({theme = get_dropdown})<CR>]],
                "Open recent file",
            },
        }, 
        s = {
            name = "search",
            b = {
                [[<Cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]],
                "Search buffer",
            },
            g = {
                [[<Cmd>lua require('telescope.builtin').live_grep()<CR>]],
                "Grep",
            },
        },

    },
    {
        mode = "n",
        prefix = "<Leader>",
    }
)

-- map('n', '<Leader>bn', '<Cmd>bnext<CR>', mopts)
-- map('n', '<Leader>bp', '<Cmd>bprev<CR>', mopts)
-- map('n', '<Leader>bd', '<Cmd>bdelete<CR>', mopts)
-- map('n', '<Leader>bB', [[<Cmd>lua require('telescope.builtin').buffers({show_all_buffers = true})<CR>]], mopts)
-- map('n', '<Leader>bg', '<Cmd>Telescope current_buffer_fuzzy_find<CR>', mopts)

-- which_key_leader['b'] = {name = '+buffers'}
-- which_key_leader['b']['n'] = 'next'
-- which_key_leader['b']['p'] = 'prev'
-- which_key_leader['b']['d'] = 'del'
-- which_key_leader['b']['B'] = 'buffers'
-- which_key_leader['b']['g'] = 'fuzzy find'


-- File mappings

-- map('n', '<Leader>bB', [[<Cmd>lua require('telescope.builtin').buffers({sort_mru = true})<CR>]], {noremap = true, silent = true})
-- map('n', '<Leader>ff', [[<Cmd>lua require('telescope.builtin').find_files({theme = get_dropdown})<CR>]], {noremap = true, silent = true})
-- map('n', '<Leader>s', [[<Cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]], {noremap = true, silent = true})
-- map('n', '<Leader>fv', '<Cmd>Telescope git_files<CR>', mopts)
-- map('n', '<Leader>fg', '<Cmd>Telescope live_grep<CR>', mopts)
-- map('n', '<Leader>fo', [[<Cmd>lua require('telescope.builtin').oldfiles()<CR>]], {noremap = true, silent = true})
-- map('n', '<Leader>ft', '<Cmd>NvimTreeOpen<CR>', mopts)
-- map('n', '<Leader>ftt', '<Cmd>NvimTreeToggle<CR>', mopts)

-- which_key_leader['f'] = {
-- 	name = '+files',
-- 	f = 'find',
-- 	v = 'find git',
-- 	g = 'grep',
-- 	t = 'tree open',
-- 	-- t = 'tree toggle'
-- }


-- LSP mappings

map('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', mopts)
map('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', mopts)
map('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', mopts)
map('n', 'gi', '<Cmd>lua vim.lsp.buf.implementation()<CR>', mopts)


-- nvim-compe

vim.api.nvim_set_keymap('i', '<C-Space>', [[compe#complete()]], {noremap = true, silent = true, expr = true})
vim.api.nvim_set_keymap('i', '<CR>', [[compe#confirm('<CR>')]], {noremap = true, silent = true, expr = true})
vim.api.nvim_set_keymap('i', '<C-e>', [[compe#close('<C-e>')]], {noremap = true, silent = true, expr = true})
vim.api.nvim_set_keymap('i', '<C-f>', [[compe#scroll({'delta': +4})]], {noremap = true, silent = true, expr = true})
vim.api.nvim_set_keymap('i', '<C-d>', [[compe#scroll({'delta': -4})]], {noremap = true, silent = true, expr = true})

-- vim-which-key

-- map('n', '<Leader>', '<Cmd>WhichKey "<Space>"<CR>', {noremap = true, silent = true})
-- vim.api.nvim_call_function('which_key#register', {'<Space>', 'g:which_key_leader'})
-- vim.api.nvim_set_var('which_key_leader', which_key_leader)
