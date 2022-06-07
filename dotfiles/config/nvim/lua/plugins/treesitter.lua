require("nvim-treesitter.configs").setup({
  playground = { enable = true },
  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = { "BufWrite", "CursorHold" },
  },
  -- breaking changes announcements
  -- https://github.com/nvim-treesitter/nvim-treesitter/issues/2293
  ensure_installed = "all",
  ignore_install = { "phpdoc" }, -- List of parsers to ignore installing
  sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { "" }, -- list of language that will be disabled
  },
  indent = { enable = true, disable = { "yaml" } }
})
