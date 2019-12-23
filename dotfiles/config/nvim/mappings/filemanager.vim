" {{{ nerd tree
map <M-1> :NERDTreeToggle<CR>

" if last window open is nerd tree close vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" }}}