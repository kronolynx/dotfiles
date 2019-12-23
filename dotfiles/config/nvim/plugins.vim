"     ____             __ _                      
"   / ___|___  _ __  / _(_) __ _ _   _ _ __ ___ 
"  | |   / _ \| '_ \| |_| |/ _` | | | | '__/ _ \
"  | |__| (_) | | | |  _| | (_| | |_| | | |  __/
"   \____\___/|_| |_|_| |_|\__, |\__,_|_|  \___|
"                          |___/                
"   ____            _                         
"  |  _ \ __ _  ___| | ____ _  __ _  ___ _ __ 
"  | |_) / _` |/ __| |/ / _` |/ _` |/ _ \ '__|
"  |  __/ (_| | (__|   < (_| | (_| |  __/ |   
"  |_|   \__,_|\___|_|\_\__,_|\__, |\___|_|   
"                             |___/           

" install vim plug if not installed
if has('nvim')
  if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !echo '[Downloading vim-plug for neovim] ...'
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs 
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
else
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !echo '[Downloading vim-plug for vim] ...'
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs 
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
endif


call plug#begin('~/.vim/plugged')

"    __ _ _      _                            _             _
"   / _(_) | ___| |_ _   _ _ __   ___   _ __ | |_   _  __ _(_)_ __  ___
"  | |_| | |/ _ \ __| | | | '_ \ / _ \ | '_ \| | | | |/ _` | | '_ \/ __|
"  |  _| | |  __/ |_| |_| | |_) |  __/ | |_) | | |_| | (_| | | | | \__ \
"  |_| |_|_|\___|\__|\__, | .__/ \___| | .__/|_|\__,_|\__, |_|_| |_|___/
"                    |___/|_|          |_|            |___/
"  
" These plugins have to be required on every boot so that filetypes
" exist and other plugins can kick in
Plug 'pangloss/vim-javascript'

"    ____                 ____  _             _
"   / ___|___  _ __ ___  |  _ \| |_   _  __ _(_)_ __  ___
"  | |   / _ \| '__/ _ \ | |_) | | | | |/ _` | | '_ \/ __|
"  | |__| (_) | | |  __/ |  __/| | |_| | (_| | | | | \__ \
"   \____\___/|_|  \___| |_|   |_|\__,_|\__, |_|_| |_|___/
"                                       |___/
"  
" Most likely plugins that I would want in every session I start

" Utility Things
Plug 'yuttie/comfortable-motion.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'machakann/vim-highlightedyank'
Plug 'janko/vim-test'
Plug 'SirVer/ultisnips'
Plug 'kana/vim-textobj-user'
" Fixers, completion and navigation
Plug 'neomake/neomake'
Plug 'w0rp/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'liuchengxu/vista.vim'
Plug 'ervandew/supertab'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'liuchengxu/vim-clap'
" Status and UI
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'itchyny/lightline.vim'
Plug 'itchyny/calendar.vim'
Plug 'glacambre/firenvim'
" Git
Plug 'tpope/vim-fugitive'
Plug 'rhysd/git-messenger.vim'
Plug 'airblade/vim-gitgutter'
Plug 'shumphrey/fugitive-gitlab.vim'
Plug 'justinmk/vim-sneak'
"   _____ _                              
"  |_   _| |__   ___ _ __ ___   ___  ___ 
"    | | | '_ \ / _ \ '_ ` _ \ / _ \/ __|
"    | | | | | |  __/ | | | | |  __/\__ \
"    |_| |_| |_|\___|_| |_| |_|\___||___/
"                                        
Plug 'challenger-deep-theme/vim' ", { 'name': 'challenger-deep' }
Plug 'ayu-theme/ayu-vim' ", {'type': 'opt'}
Plug 'drewtempelmeyer/palenight.vim' ", {'type': 'opt'}
Plug 'arcticicestudio/nord-vim' ", {'type': 'opt'}
Plug 'dracula/vim' ", {'type': 'opt', 'name': 'dracula'}
Plug 'sainnhe/lightline_foobar.vim' ", { 'type': 'opt' }
Plug 'rakr/vim-two-firewatch' ", { 'type': 'opt' }

call plug#end()
