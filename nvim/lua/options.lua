-- Global options

local o = vim.o

o.completeopt = 'menuone,noselect'
o.listchars = 'tab:→ ,trail:·,eol:¬'
o.scrolloff = 4
o.shiftwidth = 4
o.tabstop = 4
o.splitright = true
o.showmode = false
if vim.fn.has("termguicolors") then o.termguicolors = true end

-- Window-local options

local wo = vim.wo

wo.foldmethod = 'marker'
wo.number = true
wo.relativenumber = true
wo.wrap = false

-- Buffer-local options

local bo = vim.bo

bo.expandtab = true
