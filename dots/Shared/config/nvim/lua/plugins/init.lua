vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end
vim.api.nvim_command("packadd packer.nvim")
-- returns the require for use in `config` parameter of packer's use
-- expects the name of the config file
function get_setup(name)
  return string.format('require("plugins/%s")', name)
end

return require("packer").startup({
  function(use)
    use({ "wbthomason/packer.nvim" })
    use({ "kevinhwang91/nvim-bqf", ft = 'qf' }) -- TODO review if should keep
    use({ "duane9/nvim-rg" })
    use({
      "lewis6991/gitsigns.nvim",
      requires = 'nvim-lua/plenary.nvim',
      -- config = [[require('config.gitsigns')]],
      config = function()
        require('gitsigns').setup()
      end,
    })
    use({
      "iamcco/markdown-preview.nvim",
      run = function() vim.fn["mkdp#util#install"]() end,
      ft = { "markdown" },
    })
    use({
      "lukas-reineke/indent-blankline.nvim",
      config = get_setup("indent_blankline")
    })
    use({
      "nvim-treesitter/nvim-treesitter",
      run = function()
        local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
        ts_update()
      end,
      requires = {
        'nvim-treesitter/nvim-treesitter-refactor',
      },
      config = get_setup("treesitter")
    })
    use({ "nvim-treesitter/playground" })
    use({ "tpope/vim-surround" })
    use({ "tpope/vim-repeat" })
    use({ 'mtdl9/vim-log-highlighting' })
    use({
      "norcalli/nvim-colorizer.lua",
      config = get_setup("colorizer")
    })
    -- use({ "liuchengxu/vista.vim" })
    -- use({ 'stevearc/gkeep.nvim', run = ':UpdateRemotePlugins' })
    -- use({ "airblade/vim-gitgutter" })
    -- use({ 'rcarriga/nvim-notify' })
    use({ 'vigoux/notifier.nvim' })
    use({ 'stevearc/dressing.nvim' })
    use(
      {
        "nvim-telescope/telescope.nvim",
        requires = {
          { "nvim-lua/popup.nvim" },
          { "nvim-lua/plenary.nvim" },
          { 'nvim-telescope/telescope-ui-select.nvim' },
          -- { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
        },
        config = get_setup("telescope")
      })
    use({
      "nvim-telescope/telescope-frecency.nvim",
      after = 'telescope.nvim',
      requires = { "kkharji/sqlite.lua" }
    })
    use({
      "nvim-telescope/telescope-file-browser.nvim",
      requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
      config = get_setup("telescope_file_browser")
    })

    -- Another markdown plugin
    use({ "plasticboy/vim-markdown", ft = { "markdown" } })


    -- Vim tabular plugin for manipulate tabular, required by markdown plugins
    use({ "godlygeek/tabular", cmd = { "Tabularize" } })

    -- Markdown JSON header highlight plugin
    use({ "elzr/vim-json", ft = { "json", "markdown" } })

    use({ "tpope/vim-fugitive" })
    use({ "tpope/vim-vinegar" }) -- browse files commands (-)
    use {
      'junegunn/fzf.vim',
      run = function()
        vim.fn['fzf#install']()
      end
    }
    use({
      "windwp/nvim-autopairs",
      after = "nvim-cmp",
      config = get_setup("autopairs")
    })

    use({ 'easymotion/vim-easymotion' }) --  <leader><leader>w|b|W|B|e|E|j|k
    use({
      -- TOOD replace with nnn https://github.com/luukvbaal/nnn.nvim
      'kyazdani42/nvim-tree.lua',
      requires = { "nvim-lua/plenary.nvim" },
      -- event = "BufReadPre",
      requires = {
        'kyazdani42/nvim-web-devicons', -- optional, for file icon
      },
      config = get_setup('tree')
    })
    use({ "akinsho/toggleterm.nvim", config = get_setup("toggleterm") })

    use({ "ckipp01/nvim-jvmopts" })
    use({
      "hrsh7th/nvim-cmp",
      requires = {
        { "hrsh7th/cmp-buffer",                after = "nvim-cmp" },
        { "hrsh7th/cmp-nvim-lsp" },
        { "hrsh7th/cmp-path",                  after = "nvim-cmp" },
        { "onsails/lspkind.nvim" },
        { "lukas-reineke/cmp-under-comparator" },
        -- { 'hrsh7th/cmp-nvim-lsp-document-symbol', after = 'nvim-cmp' },
        -- { "hrsh7th/cmp-nvim-lsp-signature-help" },
      },
      config = get_setup("cmp"),
    })
    use({
      "RRethy/nvim-treesitter-endwise"
    })
    use({
      'scalameta/nvim-metals',
      requires = { "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap", "nvim-telescope/telescope.nvim" }
    })
    use { "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" } }
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
    })
    use({ "terryma/vim-multiple-cursors" })
    use { "williamboman/mason.nvim" }
    use({
      "neovim/nvim-lspconfig",
      wants = {
        -- "nvim-lsp-installer",
        "mason.nvim",
        "mason-lspconfig.nvim",
        "mason-tool-installer.nvim",
      },
      config = get_setup("lsp"),
      requires = {
        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim",
        "WhoIsSethDaniel/mason-tool-installer.nvim",
      }
    })
    -- Completion and linting
    use({ 'folke/trouble.nvim' })
    use({ "ray-x/lsp_signature.nvim" })
    use { 'kosayoda/nvim-lightbulb',
      config = function()
        require("nvim-lightbulb").setup({
          autocmd = { enabled = true }
        })
      end
    }
    -- use { 'mhartington/formatter.nvim' }

    use({ "sheerun/vim-polyglot" })
    use({
      "folke/which-key.nvim",
      config = get_setup("whichkey")
    })

    use {
      "luukvbaal/nnn.nvim",
      config = function() require("nnn").setup() end
    }
    --   _____ _
    --  |_   _| |__   ___ _ __ ___   ___  ___
    --    | | | '_ \ / _ \ '_ ` _ \ / _ \/ __|
    --    | | | | | |  __/ | | | | |  __/\__ \
    --    |_| |_| |_|\___|_| |_| |_|\___||___/
    --
    use({
      'nvim-lualine/lualine.nvim',
      config = get_setup("lualine"),
      event = "VimEnter",
      requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    })
    use({
      "folke/tokyonight.nvim",
      config = get_setup("theme"),
    })

    if packer_bootstrap then
      require("packer").sync()
    end
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    },
    profile = {
      enable = true,
      threshold = 1, -- the amount in ms that a plugins load time must be over for it to be included in the profile
    }
  }
})
