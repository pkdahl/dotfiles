-- Bootstrap Paq
-- https://github.com/wbthomason/packer.nvim#bootstrapping

local paq_path = vim.fn.stdpath('data') .. '/site/pack/paqs/opt/paq-nvim'

if vim.fn.empty(vim.fn.glob(paq_path)) > 0 then
  vim.cmd('silent !git clone https://github.com/savq/paq-nvim.git ' .. paq_path)
end

-- https://github.com/wbthomason/packer.nvim#bootstrapping
local packer_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
print(packer_path)
if vim.fn.empty(vim.fn.glob(packer_path)) > 0 then
  packer_bootstrap = vim.fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', packer_path})
end

return require('packer').startup(function(use)

  -- Completion
  use {
	  'hrsh7th/nvim-cmp',
	  config = function()
          local cmp = require('cmp')
		  cmp.setup({
              mapping = {
                  ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
                  ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
                  ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
                  ['<C-e>'] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close(), }),
                  ['<CR>'] = cmp.mapping.confirm({ select = true }),
              },
			  requires = {
				  'hrsh7th/cmp-buffer',
				  'hrsh7th/cmp-nvim-lsp',
				  'hrsh7th/cmp-path',
			  },
			  sources = {
				  {name = 'buffer'},
				  {name = 'nvim_lsp'},
                  {name = 'orgmode'},
				  {name = 'path'},
			  },
		  })
	  end,
   }
  -- use {'hrsh7th/cmp-nvim-lsp', after= {'hrsh7th/nvim-cmp'}}
  -- use {'hrsh7th/cmp-buffer', after= {'hrsh7th/nvim-cmp'}}
  -- use {'hrsh7th/cmp-path', after= {'hrsh7th/nvim-cmp'}}

  -- Search
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
    },
    -- setup = [[require('config.telescope_setup')]],
    -- config = [[require('config.telescope')]],
    cmd = 'Telescope',
    module = 'telescope',
  }

  -- Language server
  use {'neovim/nvim-lspconfig'}

  -- Tree-sitter
  use {
      'nvim-treesitter/nvim-treesitter',
      setup = function()
	      local ts_org_path = vim.fn.stdpath('data') .. '/site/pack/packer/opt/tree-sitter-org'
          local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
          parser_config.org = {
              install_info = {
				  -- some issue with NeoVim/tar/Apple M1 so clone and install from local instead
                  -- url = 'https://github.com/milisims/tree-sitter-org',
				  url = '~/.local/share/nvim/site/pack/packer/opt/tree-sitter-org',
                  -- revision = 'f110024d539e676f25b72b7c80b0fd43c34264ef',
                  files = {'src/parser.c', 'src/scanner.cc'},
              },
              filetype = 'org',
          }

         require('nvim-treesitter.configs').setup {
             highlight = {
                 enable = true,
                 disable = {'org'},
                 additional_vim_regex_highlighting = {'org'},
             },
             ensure_installed = {'org'},
         }
      end,
      run = ':TSUpdate'}

  -- Git

  -- Notes
  use {
    'nvim-orgmode/orgmode',
    requires = {'nvim-treesitter'},
    config = function()
      require('orgmode').setup{}
    end,
  }

  -- Mappings
  use {'folke/which-key.nvim'}

  -- Visual
  use {'arcticicestudio/nord-vim'}
  use {'itchyny/lightline.vim'}

  -- Automatically set up your configuration after cloning packer.nvim
  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- Setup Paq

-- vim.cmd('packadd paq-nvim')
-- local paq = require('paq-nvim').paq
-- paq {'savq/paq-nvim', opt=true}

-- Packages

-- paq {'arcticicestudio/nord-vim'}
-- paq {'godlygeek/tabular'}            	-- For vim-markdown
-- paq {'hrsh7th/nvim-compe'}
-- paq {'hrsh7th/nvim-cmp'}
-- paq {'hrsh7th/cmp-nvim-lsp'}
-- paq {'hrsh7th/cmp-path'}
-- paq {'hrsh7th/cmp-buffer'}
-- paq {'kyazdani42/nvim-web-devicons'}	-- File icons for nvim-tree
-- paq {'kyazdani42/nvim-tree.lua'}
-- paq {'neovim/nvim-lspconfig'}
-- paq {'nvim-lua/completion-nvim'}
-- paq {'nvim-lua/popup.nvim'}				-- Telescope dependency
-- paq {'nvim-lua/plenary.nvim'}			-- Telescope dependency
-- paq {'nvim-telescope/telescope.nvim'}
-- paq {'nvim-treesitter/nvim-treesitter'}
-- paq {'plasticboy/vim-markdown'}
-- paq {'tpope/vim-commentary'}
-- paq {'liuchengxu/vim-which-key'}
-- paq {'itchyny/lightline.vim'}
-- paq {'vhyrro/neorg'}
