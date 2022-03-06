local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  print("Packer not found")
  print("Downloading packer.nvim...")
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd [[packadd packer.nvim]] -- packadd packer module

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

return require("packer").startup(function(use)
  use({ "ckipp01/nvim-jvmopts" })
  use({
    "hrsh7th/nvim-cmp",
    requires = {
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-vsnip" },
      { "hrsh7th/vim-vsnip" },
    },
    config = require("core.plugins.cmp").setup(),
  })
  use({ "kevinhwang91/nvim-bqf" })
  use({ "kyazdani42/nvim-web-devicons" })
  use({ "liuchengxu/vista.vim" })
  use({
    "lukas-reineke/indent-blankline.nvim",
    config = require("core.plugins.indent_blankline").setup()
  })
  use({ "tpope/vim-surround" })
  use({ "neovim/nvim-lspconfig" })
  use({ "norcalli/nvim-colorizer.lua" })
  use({
    "nvim-telescope/telescope.nvim",
    requires = {
      { "nvim-lua/popup.nvim" },
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-fzy-native.nvim" },
    },
    config = require("core.plugins.telescope").setup(),
  })
  use({
    "nvim-treesitter/nvim-treesitter",
    run = ':TSUpdate',
    config = require("core.plugins.treesitter").setup()
   })
  use({ "nvim-treesitter/playground" })
  use({ "tpope/vim-fugitive" })
  use({ "tpope/vim-vinegar" }) -- browse files commands (-)
  use({ "wbthomason/packer.nvim" })
  use({
    "windwp/nvim-autopairs",
    config = require("nvim-autopairs").setup()
  })

  use({ 'easymotion/vim-easymotion' }) --  <leader><leader>w|b|W|B|e|E
  use {
      'kyazdani42/nvim-tree.lua',
      requires = {
        'kyazdani42/nvim-web-devicons', -- optional, for file icon
      },
      config = function() require'nvim-tree'.setup {} end
  }
  -- 
  use({
    'scalameta/nvim-metals',
    requires = { "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap" }
  })
  use({
    "ckipp01/scala-utils.nvim",
    requires = { "nvim-lua/plenary.nvim" }
  })
  use({
    "RRethy/vim-illuminate",
    event = "CursorHold",
    module = "illuminate",
    config = function()
      vim.g.Illuminate_delay = 1000
    end,
  })
  use({
    "andymass/vim-matchup",
    event = "CursorMoved",
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "status_manual" }
    end,
  })
  use({ "sheerun/vim-polyglot" })
  use({
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      }
    end
  })

  --   _____ _
  --  |_   _| |__   ___ _ __ ___   ___  ___
  --    | | | '_ \ / _ \ '_ ` _ \ / _ \/ __|
  --    | | | | | |  __/ | | | | |  __/\__ \
  --    |_| |_| |_|\___|_| |_| |_|\___||___/
  --
  use({
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  })
  use({ "folke/tokyonight.nvim" })

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end
end)
