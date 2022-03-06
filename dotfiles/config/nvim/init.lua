--=================================
--
-- My Neovim Setup
--
--=================================
if vim.fn.has "nvim-0.6.1" ~= 1 then
  vim.notify("Please upgrade your Neovim base installation to v0.6.1+", vim.log.levels.WARN)
  vim.wait(5000, function()
    return false
  end)
  vim.cmd "cquit"
end

local cmd = vim.cmd
local g = vim.g

-- NOTE do this ASAP since some of the stuff in our basic setup uses leader
g["mapleader"] = " "

local f = require("core.functions")
local map = f.map
local opt = vim.opt
local global_opt = vim.opt_global

--================================
-- Basic setup
--================================

require("core.plugins")
require("core.globals")
require("core.statusline")

require("core.lsp").setup()
require("core.diagnostic").setup()

--================================
-- VARIABLES ---------------------
--================================
g["netrw_gx"] = "<cWORD>"

-- plugin variables
-- polyglot's markdown settings
g["vim_markdown_conceal"] = 0
g["vim_markdown_conceal_code_blocks"] = 0

--================================
-- OPTIONS -----------------------
--================================
local indent = 2

-- global
global_opt.shortmess:remove("F"):append("c")
global_opt.termguicolors = true
global_opt.hidden = true
global_opt.showtabline = 1
global_opt.updatetime = 300
global_opt.showmatch = true
global_opt.wildignore = { ".git", "*/node_modules/*", "*/target/*", ".metals", ".bloop", ".ammonite" }
global_opt.clipboard = "unnamedplus" -- copy to clipboard
global_opt.completeopt = { "menu", "menuone", "noinsert", "noselect" }
global_opt.scrolloff = 5

-- seach
global_opt.hlsearch = true   -- Highlight searches
global_opt.ignorecase = true -- Ignore case of searches
global_opt.incsearch = true  -- Highlight dynamically as pattern is typed
global_opt.smartcase = true  -- if search has uppercase then use case sensitive search

-- indentation
global_opt.autoindent = true  -- Keep indentation from previous line
global_opt.expandtab = true   -- Expand tabs to spaces
global_opt.shiftround = true  -- When shifting lines, round the indentation to the nearest multiple of “shiftwidth.
global_opt.shiftwidth = 2     -- Indent by 2 spaces when using >>, <<, == etc.
global_opt.softtabstop = 2    -- Indent by 2 spaces when pressing <TAB>
global_opt.smartindent = true -- Automatically inserts indentation in some cases

-- performance
global_opt.lazyredraw = true -- Don’t update screen during macro and script execution.

-- interface
global_opt.laststatus = 2 -- Always show the status line
global_opt.background = "dark" -- Use colors that suit a dark background.
global_opt.cmdheight = 1 -- Height of the command bar
global_opt.cursorline = true -- Highlight current line
global_opt.guifont = "FantasqueSansMono Nerd Font 11"
global_opt.laststatus = 2 -- Always show the status line
global_opt.mouse = "a" -- Enable mouse for scrolling and resizing.
global_opt.nu = true -- line number
global_opt.ruler = true --Always show current position
global_opt.shortmess = "atI" -- Don’t show the intro message when starting Vim
global_opt.showcmd = true -- Show the (partial) command as it’s being typed
global_opt.showmatch = true -- Show matching brackets when text indicator is over them
global_opt.tabpagemax = 50 -- Maximum number of tab pages that can be opened from the command line.
global_opt.textwidth = 80 -- Make it obvious where 80 characters is
-- TODO fix the following 
-- global_opt.t_Co = 256 -- Use 256 colours (Use this setting only if your terminal supports 256 colours)
-- global_opt.noshowmode = true -- lightline shows the status not vim
-- global_opt.colorcolumn = global_opt.colorcolumn + 1 -- Make it obvious where 80 characters is
-- global_opt.noerrorbells = true -- Disable beep on errors.
-- global_opt.nostartofline = true -- Don’t reset cursor to start of line when moving around.

-- window-scoped
opt.wrap = false
opt.cursorline = true
opt.signcolumn = "yes"


-- buffer-scoped
opt.tabstop = indent
opt.shiftwidth = indent
opt.softtabstop = indent
opt.expandtab = true
opt.fileformat = "unix"

-- misc
global_opt.completeopt = {'menu', 'menuone', 'preview', 'noselect', 'noinsert' }


-- statusline
-- opt.statusline = "%!luaeval('Super_custom_status_line()')"

-- MAPPINGS -----------------------
map("i", "jj", "<ESC>")
map("v", "uu", "<ESC>")

-- Yank from cursor to end of line
map("", "Y", "y$")
-- save
map("", "<C-s>", "<esc>:w!<cr>" )

-- Sudo write
map("", "<leader>xs", ":w !sudo tee %<CR>", { noremap = true })
-- Clear last search (,qs)
map("", "<BS>", ":nohlsearch<CR>", {silent = true})

map("n", "<leader>fo", ":copen<cr>")
map("n", "<leader>fc", ":cclose<cr>")
map("n", "<leader>fn", ":cnext<cr>")
map("n", "<leader>fp", ":cprevious<cr>")

-- Smart way to move between windows
map("", "<C-j>", "<C-W>j")
map("", "<C-k>", "<C-W>k")
map("", "<C-h>", "<C-W>h")
map("", "<C-l>", "<C-W>l")
map("", "<A-,>", ":vertical res -5<cr>")
map("", "<A-.>", ":vertical res +5<cr>")
map("", "<A-lt>", ":res -5<cr>")
map("", "<A->> ", ":res +5<cr>")

-- Make many of the jump commands also center on search term
map("n", "n", "nzz", { noremap = true })
map("n", "N", "Nzz", { noremap = true })
map("n", "<C-o>", "<C-o>zz", { noremap = true })
map("n", "<C-i>", "<C-i>zz", { noremap = true })
map("n", "*", "*zz", { noremap = true })
map("n", "#", "#zz", { noremap = true })
-- This unsets the "last search pattern" register by hitting return
map("n", "<CR>", ":noh<CR><CR>", { noremap = true })

-- nvim tree
map("n", "<M-1>", ":NvimTreeToggle<CR>", { noremap = true })

-- scala-utils
map("n", "<leader>slc", [[<cmd>lua RELOAD("scala-utils.coursier").complete_from_line()<CR>]])
map("n", "<leader>sc", [[<cmd>lua RELOAD("scala-utils.coursier").complete_from_input()<CR>]])

-- other stuff
-- map("n", "<leader><leader>p", [[<cmd>lua require("core.playground.functions").peek()<CR>]])
-- map("n", "<leader><leader>s", [[<cmd>lua RELOAD("core.playground.semantic").generate()<CR>]])
-- map("n", "<leader><leader>m", [[<cmd>lua RELOAD("core.playground.mt").get_dep()<CR>]])
-- map("n", "<leader><leader>e", [[:luafile %<CR>]])
-- map("n", "<leader><leader>v", [[<cmd>lua RELOAD("core.playground.functions").get_latest_metals()<CR>]])
-- map("n", "<leader><leader>j", [[<cmd>lua RELOAD("jenkinsfile_linter").validate()<CR>]])
-- map("n", "<leader><leader>hl", [[<cmd>lua RELOAD("core.playground.functions").get_hl_under_cursor()<CR>]])
-- 
-- map("n", "<leader><leader>n", [[<cmd>lua RELOAD("core.functions").toggle_nums()<CR>]])
-- map("n", "<leader><leader>c", [[<cmd>lua RELOAD("core.functions").toggle_conceal()<CR>]])
-- map("n", "<leader><leader>jc", [[<cmd>lua RELOAD("core.functions").replace_java_converters()<CR>]])

--================================
-- COMMANDS ----------------------
--================================
cmd([[autocmd FileType markdown setlocal textwidth=80]])
cmd(
  [[autocmd BufReadPost,BufNewFile *.md,*.txt,COMMIT_EDITMSG set wrap linebreak nolist spell spelllang=en_us complete+=kspell]]
)
cmd([[autocmd BufReadPost,BufNewFile .html,*.txt,*.md,*.adoc set spell spelllang=en_us]])

-- Return to last edit position when opening files (You want this!)
cmd([[au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif]])

cmd("colorscheme tokyonight")

-- stuff
cmd([[au TextYankPost * silent! lua vim.highlight.on_yank()]]) -- yank highlight
cmd([[au FocusLost * silent! :wa]])

-- When vimwindow is resized resize splits
cmd([[au VimResized * exe "normal! \<c-w>="]])

-- source vim on save
cmd([[
    augroup autosourcing
      autocmd!
      autocmd BufWritePost .vimrc source %
    augroup end
  ]])

-- Statusline specific highlights
-- local tokyonight_colors = require("tokyonight.colors").setup()
-- cmd(string.format([[hi! StatusLine guifg=%s guibg=%s]], kanagaw_colors.fujiGray, kanagaw_colors.sumiInk1))
-- cmd([[hi! link StatusLineNC Comment]])
-- cmd([[hi! link StatusError DiagnosticError]])
-- cmd([[hi! link StatusWarn DiagnosticWarn]])

-- cmd([[autocmd TextYankPost * silent! lua vim.highlight.on_yank {}]])
--


--
local wk = require("which-key")

wk.register({
    w = {
      name = "+windows" ,
      w = { "<C-W>w"     , "other-window" },
      d = { "<C-W>c"     , "delete-window" },
      ["-"] = { "<C-W>s"     , "split-window-below" },
      ["|"] = { "<C-W>v"     , "split-window-right" },
      ["2"] = { "<C-W>v"     , "layout-double-columns" },
      h = { "<C-W>h"     , "window-left" },
      j = { "<C-W>j"     , "window-below" },
      l = { "<C-W>l"     , "window-right" },
      k = { "<C-W>k"     , "window-up" },
      H = { "<C-W>5<"    , "expand-window-left" },
      J = { "resize +5"  , "expand-window-below" },
      L = { "<C-W>5>"    , "expand-window-right" },
      K = { "resize -5"  , "expand-window-up" },
      ["="] =  { "<C-W>="     , "balance-window" },
      s = { "<C-W>s"     , "split-window-below" },
      v = { "<C-W>v"     , "split-window-below" },
      ["?"] = { "Windows"    , "fzf-window" }
    }
  }, { prefix = "<leader>" })
