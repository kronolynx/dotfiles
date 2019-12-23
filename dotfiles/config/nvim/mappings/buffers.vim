" windows {{{
" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
" }}}

" tabs {{{
"map <leader>te :tabedit
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove<cr>
map <leader>tn :tabnew<cr>
" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader><tab> :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()
" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/
" }}}

" buffers {{{
" next buffer
nnoremap <A-l> :bnext<cr>
" previous buffer
nnoremap <A-h> :bprevious<cr>
" Close the current buffer
map <leader>bd :bd<cr>
" Close all the buffers
map <leader>ba :bufdo bd<cr>
" nextm previos buffer
map <leader>bn :bnext<cr>
map <leader>bp :bprevious<cr>
" Quickly open a buffer for scribble
map <leader>q :e ~/buffer<cr>
" Quickly open a markdown buffer for scribble
map <leader>x :e ~/buffer.md<cr>
" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>
" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry
" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" }}}