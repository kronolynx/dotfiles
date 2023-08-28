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

local function map(mode, lhs, rhs, opts)
  local options = { noremap = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opt = vim.opt
local o = vim.o
local global_opt = vim.opt_global

--================================
-- Basic setup
--================================

require("plugins")

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
global_opt.termguicolors = true
global_opt.hidden = true
global_opt.showtabline = 1
global_opt.updatetime = 300
global_opt.wildignore = { ".git", "*/node_modules/*", "*/target/*", ".metals", ".bloop", ".ammonite" }
global_opt.completeopt = { "menu", "menuone", "noinsert", "noselect" }
global_opt.scrolloff = 5


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

opt.backup = false      -- don't use backup files
opt.writebackup = false -- don't backup the file while editing
opt.swapfile = false    -- don't create swap files for new buffers
opt.updatecount = 0     -- don't write swap files after some number of updates

opt.backupdir = {
  "~/.vim-tmp",
  "~/.tmp",
  "~/tmp",
  "/var/tmp",
  "/tmp"
}

opt.directory = {
  "~/.vim-tmp",
  "~/.tmp",
  "~/tmp",
  "/var/tmp",
  "/tmp"
}

opt.history = 1000                         -- store the last 1000 commands entered
-- opt.textwidth = 120 -- after configured number of characters, wrap line
opt.backspace = { "indent", "eol,start" }  -- make backspace behave in a sane manner
opt.clipboard = { "unnamed", "unnamedplus" } -- use the system clipboard
opt.mouse = "a"                            -- set mouse mode to all modes

-- searching
opt.ignorecase = true  -- case insensitive searching
opt.smartcase = true   -- case-sensitive if expresson contains a capital letter
opt.hlsearch = true    -- highlight search results
opt.incsearch = true   -- set incremental search, like modern browsers
opt.lazyredraw = false -- don't redraw while executing macros
opt.magic = true       -- set magic on, for regular expressions

-- error bells
opt.errorbells = false
opt.visualbell = true
opt.timeoutlen = 500

-- indentation
opt.autoindent = true  -- Keep indentation from previous line
opt.expandtab = true   -- Expand tabs to spaces
opt.shiftround = true  -- When shifting lines, round the indentation to the nearest multiple of “shiftwidth.
opt.smarttab = true    -- tab respects 'tabstop', 'shiftwidth', and 'softtabstop'
opt.tabstop = 2        -- the visible width of tabs
opt.shiftwidth = 2     -- Indent by 2 spaces when using >>, <<, == etc.
opt.softtabstop = 2    -- Indent by 2 spaces when pressing <TAB>
opt.smartindent = true -- Automatically inserts indentation in some cases

-- interface
opt.laststatus = 2      -- Always show the status line
opt.background = "dark" -- Use colors that suit a dark background.
opt.cmdheight = 1       -- Height of the command bar
opt.cursorline = true   -- Highlight current line
opt.guifont = "FantasqueSansMono Nerd Font 11"
opt.laststatus = 2      -- Always show the status line
opt.ruler = true        --Always show current position
opt.shortmess = "atI"   -- Don’t show the intro message when starting Vim
opt.showcmd = true      -- Show the (partial) command as it’s being typed
opt.showmatch = true    -- Show matching brackets when text indicator is over them
opt.tabpagemax = 50     -- Maximum number of tab pages that can be opened from the command line.
o.termguicolors = true

opt.number = true    -- show line numbers
opt.wrap = false     -- turn on line wrapping
opt.linebreak = true -- set soft wrapping
opt.ttyfast = true   -- faster redrawing
opt.wildmenu = true  -- enhanced command line completion
opt.wildmode = {"longest:full", "full"} -- complete files like a shell
opt.hidden = true    -- current buffer can be put into background
opt.showcmd = true   -- show incomplete commands
opt.showmode = true  -- don't show which mode disabled for PowerLine
opt.shell = vim.env.SHELL
opt.cmdheight = 1 -- command bar height
opt.title = true  -- set terminal title
opt.mat = 2       -- how many tenths of a second to blink
opt.updatetime = 300
opt.signcolumn = "yes"
opt.shortmess = "atToOFc" -- prompt message options

-- code folding settings
cmd [[set foldmethod=expr]] -- use treesitter folding support
cmd [[set foldexpr=nvim_treesitter#foldexpr()]]
opt.foldlevelstart = 99
opt.foldnestmax = 10   -- deepest fold is 10 levels
opt.foldenable = false -- don't fold by default
opt.foldlevel = 1

-- toggle invisible characters
opt.list = true
opt.listchars = {
  tab = "→ ",
  eol = "¬",
  trail = "⋅",
  extends = "❯",
  precedes = "❮"
}

-- misc
opt.completeopt = { 'menu', 'menuone', 'preview', 'noselect', 'noinsert' }

-- MAPPINGS -----------------------
map("i", "jj", "<ESC>")
map("v", "uu", "<ESC>")

-- Yank from cursor to end of line
map("", "Y", "y$")
-- save
map("", "<C-s>", "<esc>:w!<cr>")

-- Sudo write
map("", "<leader>xs", ":w !sudo tee %<CR>", { noremap = true })
-- format json
map("n", "<leader>fj", ":%! python -m json.tool --indent=2<CR>", { silent = true })
map("v", "<leader>fj", ":'<,'>! python -m json.tool --indent=2<CR>", { silent = true })
-- Clear last search (,qs)
map("", "<BS>", ":nohlsearch<CR>", { silent = true })

map("n", "<leader>fo", ":copen<cr>")
map("n", "<leader>fc", ":cclose<cr>")
map("n", "<leader>fn", ":cnext<cr>")
map("n", "<leader>fp", ":cprevious<cr>")

map("v", "<leader>p", "\"dP")

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
map("", "<C-q>", ":NvimTreeToggle<CR>", { noremap = true })
-- telescope tree
-- map("", "<C-e>", ":Telescope file_browser<CR>", { noremap = true })
map("n", "<leader>ff", ":Telescope file_browser path=%:p:h select_buffer=true<CR>", { noremap = true })

-- scala-utils
map("n", "<leader>slc", [[<cmd>lua RELOAD("scala-utils.coursier").complete_from_line()<CR>]])
map("n", "<leader>sc", [[<cmd>lua RELOAD("scala-utils.coursier").complete_from_input()<CR>]])

-- fzf
map("", "<leader>gf", ":Telescope find_files<CR>")
map("", "<leader>tr", ":Telescope buffers<CR>")
map("", "<leader>bs", ":Telescope marks<CR>")
map("", "<leader>xk", ":Telescope keymaps<CR>")
map("", "<leader>ml", ":Telescope git_commits<CR>")
map("", "<leader>fp", ":Telescope live_grep<CR>")          -- find in path
map("", "<leader>sp", ":Telescope live_grep<CR>")          -- search in path
map("", "<M-j>", ":Commands<CR>")      -- TODO fix me

-- easymotion (hop)
map("", "<leader><leader>j", ":HopLineStartAC<CR>")
map("", "<leader><leader>k", ":HopLineStartBC<CR>")
map("", "<leader><leader>w", ":HopWordAC<CR>")
map("", "<leader><leader>b", ":HopWordBC<CR>")


-- TODO :GitGutterPreviewHunk
--  let g:gitgutter_map_keys = 0
-- map("n", "<leader>mp", ":GitGutterPreviewHunk<CR>")
-- map("n", "<leader>ms", ":GitGutterStageHunk<CR>")
-- gitsigns
map("n", "<leader>mv", ":Gitsigns preview_hunk<CR>")
map("n", "<leader>ms", ":Gitsigns stage_hunk<CR>")
map("n", "<leader>mr", ":Gitsigns reset_hunk<CR>")
map("n", "<leader>mn", ":Gitsigns next_hunk<CR>")
map("n", "<leader>mp", ":Gitsigns prev_hunk<CR>")
map("n", "<leader>mb", ":Gitsigns toggle_current_line_blame<CR>")
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

-- Format JSON files
vim.api.nvim_create_autocmd("FileType",
  {
    pattern = "json",
    command = [[nnoremap <buffer><leader>e= :%!jq .<CR>]]
    -- command = [[nnoremap <buffer><leader>e= :%!python -m json.tool<CR>]]
  }
)

cmd [[syntax on]]
cmd [[filetype plugin indent on]]
-- make the highlighting of tabs and other non-text less annoying
cmd [[highlight SpecialKey ctermfg=19 guifg=#333333]]
cmd [[highlight NonText ctermfg=19 guifg=#333333]]

-- make comments and HTML attributes italic
cmd [[highlight Comment cterm=italic term=italic gui=italic]]
cmd [[highlight htmlArg cterm=italic term=italic gui=italic]]
cmd [[highlight xmlAttrib cterm=italic term=italic gui=italic]]
-- highlight Type cterm=italic term=italic gui=italic
cmd [[highlight Normal ctermbg=none]]

-- cmd [[
-- autocmd ColorScheme * highlight ExtraWhitespace ctermbg=204 guibg=#f7768e
-- match ExtraWhitespace /\s\+$/
-- autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
-- autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
-- autocmd InsertLeave * match ExtraWhitespace /\s\+$/
-- autocmd BufWinLeave * call clearmatches()
-- ]]
vim.g.better_whitespace_ctermcolor='204'
vim.g.better_whitespace_guicolor='#f7768e'

-- cursor style
vim.opt.guicursor = 'n-v-c:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor,a:blinkon100'

function hiCursor()
  vim.api.nvim_set_hl(0, "Cursor", { fg = '#343B4C', bg = '#77ffeb' })
end

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "*",
  callback = hiCursor,
})
