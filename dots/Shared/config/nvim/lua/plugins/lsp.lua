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
local lsp_capabilities = require("cmp_nvim_lsp").default_capabilities

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
