-- local fn = vim.fn

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
--
-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup lazy_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | Lazy sync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, lazy = pcall(require, "lazy")
if not status_ok then
  return
end


return lazy.setup({
  {
    'nvim-lualine/lualine.nvim',
    -- config = get_setup("lualine"),
    config = function()
      -- Eviline config for lualine
      -- Author: shadmansaleh
      -- Credit: glepnir
      local lualine = require('lualine')

      -- Color table for highlights
      -- stylua: ignore
      local colors = {
        bg       = '#202328',
        fg       = '#bbc2cf',
        yellow   = '#ECBE7B',
        cyan     = '#008080',
        darkblue = '#081633',
        green    = '#98be65',
        orange   = '#FF8800',
        violet   = '#a9a1e1',
        magenta  = '#c678dd',
        blue     = '#51afef',
        red      = '#ec5f67',
      }

      local conditions = {
        buffer_not_empty = function()
          return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
        end,
        hide_in_width = function()
          return vim.fn.winwidth(0) > 80
        end,
        check_git_workspace = function()
          local filepath = vim.fn.expand('%:p:h')
          local gitdir = vim.fn.finddir('.git', filepath .. ';')
          return gitdir and #gitdir > 0 and #gitdir < #filepath
        end,
      }

      -- Config
      local config = {
        options = {
          -- Disable sections and component separators
          component_separators = '',
          section_separators = '',
          theme = {
            -- We are going to use lualine_c an lualine_x as left and
            -- right section. Both are highlighted by c theme .  So we
            -- are just setting default looks o statusline
            normal = { c = { fg = colors.fg, bg = colors.bg } },
            inactive = { c = { fg = colors.fg, bg = colors.bg } },
          },
        },
        sections = {
          -- these are to remove the defaults
          lualine_a = {},
          lualine_b = {},
          lualine_y = {},
          lualine_z = {},
          -- These will be filled later
          lualine_c = {},
          lualine_x = {},
        },
        inactive_sections = {
          -- these are to remove the defaults
          lualine_a = {},
          lualine_b = {},
          lualine_y = {},
          lualine_z = {},
          lualine_c = {},
          lualine_x = {},
        },
      }

      -- Inserts a component in lualine_c at left section
      local function ins_left(component)
        table.insert(config.sections.lualine_c, component)
      end

      -- Inserts a component in lualine_x ot right section
      local function ins_right(component)
        table.insert(config.sections.lualine_x, component)
      end

      ins_left {
        function()
          return '▊'
        end,
        color = { fg = colors.blue },      -- Sets highlighting of component
        padding = { left = 0, right = 1 }, -- We don't need space before this
      }

      ins_left {
        -- mode component
        function()
          return ''
        end,
        color = function()
          -- auto change color according to neovims mode
          local mode_color = {
            n = colors.red,
            i = colors.green,
            v = colors.blue,
            [''] = colors.blue,
            V = colors.blue,
            c = colors.magenta,
            no = colors.red,
            s = colors.orange,
            S = colors.orange,
            [''] = colors.orange,
            ic = colors.yellow,
            R = colors.violet,
            Rv = colors.violet,
            cv = colors.red,
            ce = colors.red,
            r = colors.cyan,
            rm = colors.cyan,
            ['r?'] = colors.cyan,
            ['!'] = colors.red,
            t = colors.red,
          }
          return { fg = mode_color[vim.fn.mode()] }
        end,
        padding = { right = 1 },
      }

      ins_left {
        -- filesize component
        'filesize',
        cond = conditions.buffer_not_empty,
      }

      ins_left {
        'filename',
        cond = conditions.buffer_not_empty,
        color = { fg = colors.magenta, gui = 'bold' },
      }

      ins_left { 'location' }

      ins_left { 'progress', color = { fg = colors.fg, gui = 'bold' } }

      ins_left {
        'diagnostics',
        sources = { 'nvim_diagnostic' },
        symbols = { error = ' ', warn = ' ', info = ' ' },
        diagnostics_color = {
          color_error = { fg = colors.red },
          color_warn = { fg = colors.yellow },
          color_info = { fg = colors.cyan },
        },
      }

      -- Insert mid section. You can make any number of sections in neovim :)
      -- for lualine it's any number greater then 2
      ins_left {
        function()
          return '%='
        end,
      }

      ins_left {
        -- Lsp server name .
        function()
          local msg = ''
          local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
          local clients = vim.lsp.get_active_clients()
          if next(clients) == nil then
            return msg
          end
          for _, client in ipairs(clients) do
            local filetypes = client.config.filetypes
            if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
              return client.name
            end
          end
          return msg
        end,
        icon = '󱙝 LSP',
        cond = conditions.hide_in_width,
        color = { fg = '#ffffff', gui = 'bold' },
      }

      -- Add components to right sections
      ins_right {
        'o:encoding',       -- option component same as &encoding in viml
        fmt = string.upper, -- I'm not sure why it's upper case either ;)
        cond = conditions.hide_in_width,
        color = { fg = colors.green, gui = 'bold' },
      }

      ins_right {
        'fileformat',
        fmt = string.upper,
        icons_enabled = false, -- I think icons are cool but Eviline doesn't have them. sigh
        color = { fg = colors.green, gui = 'bold' },
      }

      ins_right {
        'branch',
        icon = '',
        color = { fg = colors.violet, gui = 'bold' },
      }

      ins_right {
        'diff',
        symbols = { added = ' ', modified = ' ', removed = ' ' },
        diff_color = {
          added = { fg = colors.green },
          modified = { fg = colors.orange },
          removed = { fg = colors.red },
        },
        cond = conditions.hide_in_width,
      }

      ins_right {
        function()
          return '▊'
        end,
        color = { fg = colors.blue },
        padding = { left = 1 },
      }

      -- Now don't forget to initialize lualine
      lualine.setup(config)
    end,
    event = "VimEnter",
    dependencies = { 'kyazdani42/nvim-web-devicons', lazy = true }
  },
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd [[colorscheme tokyonight-storm]]
    end
  },
  { "kevinhwang91/nvim-bqf", ft = 'qf' }, -- TODO review if should keep
  { "duane9/nvim-rg" },
  {
    "lewis6991/gitsigns.nvim",
    dependencies = 'nvim-lua/plenary.nvim',
    config = function()
      require('gitsigns').setup()
    end,
  },
  {
    "iamcco/markdown-preview.nvim",
    build = function() vim.fn["mkdp#util#install"]() end,
    ft = { "markdown" },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({
        char = "⋅",
        filetype_exclude = { "help" },
      })
    end
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = function()
      local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
      ts_update()
    end,
    dependencies = {
      'nvim-treesitter/nvim-treesitter-refactor',
    },
    config = function()
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
        sync_install = false,          -- install languages synchronously (only applied to `ensure_installed`)
        highlight = {
          enable = true,               -- false will disable the whole extension
          disable = { "" },            -- list of language that will be disabled
        },
        indent = { enable = true, disable = { "yaml" } }
      })
    end
  },
  { "nvim-treesitter/playground" },
  { "tpope/vim-surround" },
  { "tpope/vim-repeat" },
  { 'mtdl9/vim-log-highlighting' },
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require('colorizer').setup()
    end
  },
  {
    'rcarriga/nvim-notify',
    config = function()
      local nvim_notify = require("notify")
      nvim_notify.setup {
        -- Animation style
        stages = "fade_in_slide_out",

        -- Default timeout for notifications
        --timeout = 3500,
        background_colour = "#2E3440",
      }
      vim.notify = nvim_notify
    end
  },
  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  },
  -- { 'Shatur/neovim-session-manager' },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      { "nvim-lua/popup.nvim" },
      { "nvim-lua/plenary.nvim" },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      -- { "nvim-telescope/telescope-fzf-native.nvim", build = "make" }
      {
        "nvim-telescope/telescope-frecency.nvim",
        dependencies = { "kkharji/sqlite.lua" }
      },
    },
    config = function()
      -- local f = require("functions")
      -- local map = f.map
      -- map("n", "<leader>ff", [[<cmd>lua require("telescope.builtin").find_files({layout_strategy="vertical"})<CR>]])
      -- map("n", "<leader>lg", [[<cmd>lua require("telescope.builtin").live_grep({layout_strategy="vertical"})<CR>]])
      -- map("n", "<leader>gh", [[<cmd>lua require("telescope.builtin").git_commits({layout_strategy="vertical"})<CR>]])
      -- map("n", "<leader>mc", [[<cmd>lua require("telescope").extensions.metals.commands()<CR>]])
      -- map("n", "<leader>cc", [[<cmd>lua RELOAD("telescope").extensions.coursier.complete()<CR>]])
      --
      -- map("n", "gds", [[<cmd>lua require("telescope.builtin").lsp_document_symbols()<CR>]])
      -- map("n", "gws", [[<cmd>lua require("telescope.builtin").lsp_dynamic_workspace_symbols()<CR>]])

      -- local actions = require("telescope.actions")
      require("telescope").setup({
        defaults = {
          file_ignore_patterns = { "target", "node_modules", "parser.c", "out", "%.min.js", "build", "logs" },
          prompt_prefix = "❯",
          file_previewer = require("telescope.previewers").vim_buffer_cat.new,
          grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
          mappings = {
            n = {
              -- ["f"] = actions.send_to_qflist,
            },
          },
        },
      })

      require("telescope").load_extension("ui-select")
      -- require("telescope").load_extension("fzf")
      require("telescope").load_extension("file_browser")
    end
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").setup {
        extensions = {
          file_browser = {
            theme = "ivy",
            -- disables netrw and use telescope-file-browser in its place
            hijack_netrw = true,
          },
        },
      }
    end
  },

  -- Another markdown plugin
  { "plasticboy/vim-markdown", ft = { "markdown" } },


  -- Vim tabular plugin for manipulate tabular, required by markdown plugins
  { "godlygeek/tabular",       cmd = { "Tabularize" } },

  -- Markdown JSON header highlight plugin
  { "elzr/vim-json",           ft = { "json", "markdown" } },

  { "tpope/vim-fugitive" },
  { "tpope/vim-vinegar" }, -- browse files commands (-)
  {
    'junegunn/fzf.vim',
    build = function()
      vim.fn['fzf#install']()
    end
  },

  {
    "phaazon/hop.nvim",
    config = function()
      require 'hop'.setup()
    end
  },
  {
    -- TOOD replace with nnn https://github.com/luukvbaal/nnn.nvim
    'kyazdani42/nvim-tree.lua',
    -- event = "BufReadPre",
    dependencies = {
      'kyazdani42/nvim-web-devicons', -- optional, for file icon
      "nvim-lua/plenary.nvim",
    },
    config = function()
      local HEIGHT_RATIO = 0.8
      local WIDTH_RATIO = 0.5

      require("nvim-tree").setup({
        disable_netrw = true,
        hijack_netrw = true,
        respect_buf_cwd = true,
        sync_root_with_cwd = true,
        view = {
          relativenumber = true,
          float = {
            enable = true,
            open_win_config = function()
              local screen_w = vim.opt.columns:get()
              local screen_h = vim.opt.lines:get() - vim.opt.cmdheight:get()
              local window_w = screen_w * WIDTH_RATIO
              local window_h = screen_h * HEIGHT_RATIO
              local window_w_int = math.floor(window_w)
              local window_h_int = math.floor(window_h)
              local center_x = (screen_w - window_w) / 2
              local center_y = ((vim.opt.lines:get() - window_h) / 2)
                  - vim.opt.cmdheight:get()
              return {
                border = "rounded",
                relative = "editor",
                row = center_y,
                col = center_x,
                width = window_w_int,
                height = window_h_int,
              }
            end,
          },
          width = function()
            return math.floor(vim.opt.columns:get() * WIDTH_RATIO)
          end,
        }
      })
    end
  },
  {
    "akinsho/toggleterm.nvim",
    config =
        function()
          require("toggleterm").setup({
            open_mapping = "<c-t>",
            hide_numbers = true, -- hide the number column in toggleterm buffers
            shade_terminals = false,
            start_in_insert = true,
            insert_mappings = true, -- whether or not the open mapping applies in insert mode
            persist_size = true,
            direction = "horizontal",
            close_on_exit = true, -- close the terminal window when the process exits
            float_opts = {
              border = "curved",
              width = 120,
              height = 40,
              winblend = 3,
              highlights = {
                border = "Normal",
                background = "Normal",
              },
            },
          })
        end
  },
  { "ckipp01/nvim-jvmopts" },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-vsnip" },
      { "hrsh7th/vim-vsnip" },
      { "onsails/lspkind.nvim" },
      { "lukas-reineke/cmp-under-comparator" },
      -- { 'hrsh7th/cmp-nvim-lsp-document-symbol'},
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      {
        "windwp/nvim-autopairs",
        config = function()
          require("nvim-autopairs").setup({})
        end
      },
    },
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        snippet = {
          expand = function(args)
            -- Comes from vsnip
            vim.fn["vsnip#anonymous"](args.body)
          end,
        },
        mapping = {
          -- None of this made sense to me when first looking into this since there
          -- is no vim docs, but you can't have select = true here _unless_ you are
          -- also using the snippet stuff. So keep in mind that if you remove
          -- snippets you need to remove this select
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end,
          ["<S-Tab>"] = function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            else
              fallback()
            end
          end,
        },
        completion = {
          completeopt = "menu,menuone,noinsert",
        },
        sources = {
          { name = "nvim_lsp",               priority = 10 },
          { name = "vsnip" },
          { name = "buffer" },
          { name = "look",                   keyword_length = 3, option = { convert_case = true, loud = true } },
          { name = "nvim_lua" },
          { name = "path" },
          { name = "nvim_lsp_signature_help" },
        },
      })
    end
  },
  {
    "RRethy/nvim-treesitter-endwise"
  },
  {
    'scalameta/nvim-metals',
    dependencies = { "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap", "nvim-telescope/telescope.nvim" }
  },
  { "rcarriga/nvim-dap-ui", dependencies = { "mfussenegger/nvim-dap" } },
  {
    "ckipp01/scala-utils.nvim",
    dependencies = { "nvim-lua/plenary.nvim" }
  },
  {
    "RRethy/vim-illuminate",
    event = "CursorHold",
    module = "illuminate",
    config = function()
      vim.g.Illuminate_delay = 1000
    end,
  },
  {
    "andymass/vim-matchup",
    event = "CursorMoved",
  },
  { "terryma/vim-multiple-cursors" },
  { "williamboman/mason.nvim" },
  { "ntpeters/vim-better-whitespace" },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local api = vim.api

      local function map(mode, lhs, rhs, opts)
        local options = { noremap = true, silent = true }
        if opts then
          options = vim.tbl_extend("force", options, opts)
        end
        api.nvim_set_keymap(mode, lhs, rhs, options)
      end

      require('mason').setup()
      require('mason-lspconfig').setup({
        ensure_installed = {
          -- Replace these with whatever servers you want to install
          -- https://github.com/williamboman/mason-lspconfig.nvim
          'rust_analyzer',
          'tsserver',
          'pyright',
          'lua_ls',
          'bashls',
          'jsonls',
          --    'hls',
          'taplo',
          'yamlls'
        }
      })

      local lspconfig = require("lspconfig")
      local lsp_capabilities = require("cmp_nvim_lsp").default_capabilities()

      require('mason-lspconfig').setup_handlers({
        function(server_name)
          -- lspconfig[server_name].setup({
          --   capabilities = lsp_capabilities,
          -- })
          lspconfig[server_name].setup({})
        end,
      })

      lspconfig.util.default_config = vim.tbl_extend("force", lspconfig.util.default_config, {
        capabilities = lsp_capabilities,
      })

      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" })

      local lsp_group = api.nvim_create_augroup("lsp", { clear = true })

      local on_attach = function(client, bufnr)
        -- LSP agnostic mappings
        map("n", "<leader>rr", [[<cmd>lua vim.lsp.buf.rename()<CR>]])
        map("n", "<leader>ca", [[<cmd>lua vim.lsp.buf.code_action()<CR>]])
        map("n", "<leader>cl", [[<cmd>lua vim.lsp.codelens.run()<CR>]])
        map("n", "<leader>=", [[<cmd>lua vim.lsp.buf.format({ async = true })<CR>]])

        map("n", "<leader>vt", [[<cmd>lua vim.lsp.buf.hover()<CR>]])
        map("n", "<leader>vp", [[<cmd>lua vim.lsp.buf.signature_help()<CR>]])
        map("n", "<leader>vs", [[<cmd>lua vim.lsp.buf.document_symbol()<CR>]])

        map("n", "<leader>gi", [[<cmd>lua vim.lsp.buf.implementation()<CR>]])
        map("n", "<leader>gr", [[<cmd>lua vim.lsp.buf.references()<CR>]])
        -- map("n", "<leader>gd", [[<cmd>lua vim.lsp.buf.definition()<CR>]])
        map("n", "<leader>gd", [[<cmd>lua vim.lsp.buf.type_definition()<CR>]])
        map("n", "<leader>gu", [[<cmd>lua vim.lsp.buf.incomming_calls()<CR>]])
        map("n", "<leader>go", [[<cmd>lua vim.lsp.buf.outgoing_calls()<CR>]])
        map("n", "<leader>fr", [[<cmd>lua vim.lsp.buf.references()<CR>]])
        map("", "<M-CR>", [[<cmd>lua vim.lsp.buf.code_action()<CR>]])
        map("", "<M-CR>", [[<cmd>lua <CR>]])
        map("n", "<leader>as", [[<cmd>lua vim.lsp.start_client()<CR>]])
        map("n", "<leader>ah", [[<cmd>lua vim.lsp.stop_client()<CR>]])

        -- TODO
        -- vim.lsp.buf.workspace_symbol()  Lists all symbols in the current workspace in the quickfix window.
        --*vim.lsp.buf.clear_references()* Removes document highlights from current buffer.
        --*vim.lsp.buf.completion()* Retrieves the completion items at the current cursor position. Can only be called in Insert mode.

        api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
      end

      --================================
      -- Metals specific setup
      --================================
      local metals_config = require("metals").bare_config()
      metals_config.tvp = {
        icons = {
          enabled = true
        }
      }

      metals_config.settings = {
        showImplicitArguments = true,
        showImplicitConversionsAndClasses = true,
        showInferredType = true,
        excludedPackages = {
          "akka.actor.typed.javadsl",
          "com.github.swagger.akka.javadsl",
          "akka.stream.javadsl",
          "akka.http.javadsl",
        },
      }

      metals_config.init_options.statusBarProvider = "on"
      metals_config.capabilities = lsp_capabilities

      metals_config.on_attach = function(client, bufnr)
        on_attach(client, bufnr)

        -- Metals specific mappings
        map("v", "<leader>vt", [[<Esc><cmd>lua require("metals").type_of_range()<CR>]])
        map("n", "<leader>ws", [[<cmd>lua require("metals").hover_worksheet({ border = "single" })<CR>]])
        map("n", "<leader>tt", [[<cmd>lua require("metals.tvp").toggle_tree_view()<CR>]])
        map("n", "<leader>ff", [[<cmd>lua require("metals.tvp").reveal_in_tree()<CR>]])
        map("n", "<leader>vis", [[<cmd>lua require("metals").toggle_setting("showImplicitArguments")<CR>]])

        -- A lot of the servers I use won't support document_highlight or codelens,
        -- so we juse use them in Metals
        api.nvim_create_autocmd("CursorHold", {
          callback = vim.lsp.buf.document_highlight,
          buffer = bufnr,
          group = lsp_group,
        })
        api.nvim_create_autocmd("CursorMoved", {
          callback = vim.lsp.buf.clear_references,
          buffer = bufnr,
          group = lsp_group,
        })
        api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave" }, {
          callback = vim.lsp.codelens.refresh,
          buffer = bufnr,
          group = lsp_group,
        })
        api.nvim_create_autocmd("FileType", {
          pattern = { "dap-repl" },
          callback = function()
            require("dap.ext.autocompl").attach()
          end,
          group = lsp_group,
        })

        -- nvim-dap
        -- I only use nvim-dap with Scala, so we keep it all in here
        local dap = require("dap")

        dap.configurations.scala = {
          {
            type = "scala",
            request = "launch",
            name = "Run or test with input",
            metals = {
              runType = "runOrTestFile",
              args = function()
                local args_string = vim.fn.input("Arguments: ")
                return vim.split(args_string, " +")
              end,
            },
          },
          {
            type = "scala",
            request = "launch",
            name = "Run or Test",
            metals = {
              runType = "runOrTestFile",
            },
          },
          {
            type = "scala",
            request = "launch",
            name = "Test Target",
            metals = {
              runType = "testTarget",
            },
          },
        }

        map("n", "<leader>dc", [[<cmd>lua require("dap").continue()<CR>]])
        map("n", "<leader>dr", [[<cmd>lua require("dap").repl.toggle()<CR>]])
        map("n", "<leader>dK", [[<cmd>lua require("dap.ui.widgets").hover()<CR>]])
        map("n", "<leader>dt", [[<cmd>lua require("dap").toggle_breakpoint()<CR>]])
        map("n", "<leader>dso", [[<cmd>lua require("dap").step_over()<CR>]])
        map("n", "<leader>dsi", [[<cmd>lua require("dap").step_into()<CR>]])
        map("n", "<leader>drl", [[<cmd>lua require("dap").run_last()<CR>]])

        -- dap.listeners.after["event_terminated"]["nvim-metals"] = function(session, body)
        --   --vim.notify("Tests have finished!")
        --   dap.repl.open()
        -- end

        require("metals").setup_dap()
      end

      local nvim_metals_group = api.nvim_create_augroup("nvim-metals", { clear = true })
      api.nvim_create_autocmd("FileType", {
        pattern = { "scala", "sbt", "java" },
        callback = function()
          vim.notify("Metals initialize")
          require("metals").initialize_or_attach(metals_config)
        end,
        group = nvim_metals_group,
      })

      lspconfig.jsonls.setup({
        on_attach = on_attach,
        commands = {
          Format = {
            function()
              vim.lsp.buf.range_formatting({}, { 0, 0 }, { vim.fn.line("$"), 0 })
            end,
          },
        },
      })

      -- -- These server just use the vanilla setup
      local servers = { "bashls", "tsserver", "yamlls", "lua_ls" }
      for _, server in pairs(servers) do
        lspconfig[server].setup({ on_attach = on_attach })
      end

      -- Uncomment for trace logs from neovim
      --vim.lsp.set_log_level('trace')
    end,
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
      {
        "j-hui/fidget.nvim",
        tag = 'legacy',
        config = function()
          require('fidget').setup()
        end
      }, -- Useful status updates for LSP
      {
        "folke/neodev.nvim",
        config = function()
          require('neodev').setup()
        end
      }, -- Additional lua configuration
    }
  },
  -- Completion and linting
  { 'folke/trouble.nvim' },
  -- { "ray-x/lsp_signature.nvim" },
  {
    'kosayoda/nvim-lightbulb',
    config = function()
      require("nvim-lightbulb").setup({
        autocmd = { enabled = true }
      })
    end
  },
  {
    'gelguy/wilder.nvim',
    config = function()
      local wilder = require('wilder')
      wilder.setup({ modes = { ':', '/', '?' } })

      wilder.set_option('renderer', wilder.popupmenu_renderer({
        highlighter = wilder.basic_highlighter(),
        max_height = 15,
        left = { ' ', wilder.popupmenu_devicons() },
        right = { ' ', wilder.popupmenu_scrollbar() },
        apply_incsearch_fix = 0,
        highlights = {
          accent = wilder.make_hl('WilderAccent', 'Pmenu', { {}, {}, { foreground = '#f4468f' } }),
        },
        border = 'rounded',
      }))
    end
  },
  -- { "sheebuild/vim-polyglot" },
  {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup({
        plugins = {
          marks = true,     -- shows a list of your marks on ' and `
          registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
          -- the presets plugin, adds help for a bunch of default keybindings in Neovim
          -- No actual key bindings are created
          spelling = {
            enabled = true,  -- enabling this will show WhichKey when pressing z= to select spelling suggestions
            suggestions = 9, -- how many suggestions should be shown in the list?
          },
          presets = {
            operators = true,    -- adds help for operators like d, y, ... and registers them for motion / text object completion
            motions = true,      -- adds help for motions
            text_objects = true, -- help for text objects triggered after entering an operator
            windows = true,      -- default bindings on <c-w>
            nav = true,          -- misc bindings to work with windows
            z = true,            -- bindings for folds, spelling and others prefixed with z
            g = true,            -- bindings for prefixed with g
          },
        },
        -- -- add operators that will trigger motion and text object completion
        -- -- to enable all native operators, set the preset / operators plugin above
        -- operators = { gc = 'Comments' },
        icons = {
          breadcrumb = '»', -- symbol used in the command line area that shows your active key combo
          separator = ' ', -- symbol used between a key and it's label
          group = '+', -- symbol prepended to a group
        },
        -- ➜  
        window = {
          border = 'none',          -- none, single, double, shadow
          position = 'bottom',      -- bottom, top
          margin = { 1, 0, 1, 0 },  -- extra window margin [top, right, bottom, left]
          padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
        },
        layout = {
          height = { min = 4, max = 25 },                                             -- min and max height of the columns
          width = { min = 20, max = 50 },                                             -- min and max width of the columns
          spacing = 3,                                                                -- spacing between columns
          align = 'left',                                                             -- align columns left, center or right
        },
        ignore_missing = false,                                                       -- enable this to hide mappings for which you didn't specify a label
        hidden = { '<silent>', '<cmd>', '<Cmd>', '<CR>', 'call', 'lua', '^:', '^ ' }, -- hide mapping boilerplate
        show_help = true,                                                             -- show help message on the command line when the popup is visible
      })
    end
  },

  {
    "luukvbaal/nnn.nvim",
    config = function() require("nnn").setup() end
  },
})

