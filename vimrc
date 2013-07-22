" Load pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" Load poweline, if installed
if exists("$POWERLINE_ROOT")
    set runtimepath+=$POWERLINE_ROOT/powerline/bindings/vim
endif

" Make Vim more useful
set nocompatible

" Load cool plugins
filetype plugin indent on

" Syntax highlighting
syntax on

" Allow unsaved background buffers
set hidden

" Temporary stuff
set nobackup
set writebackup
set noswapfile

" Tabbing
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
set smartindent
set cindent

" Windows
set splitright

" Searching
set incsearch
set nohlsearch
set ignorecase smartcase

" Theming
set cursorline
set laststatus=2
set ruler
set relativenumber
" set number
set showcmd
set guifont=Menlo:h13
let g:rehash256 = 1
colorscheme molokai
set background=dark

" Ex-mode completion
set wildmode=longest:full,full
set wildmenu

" Editing
set backspace=indent,eol,start
inoremap <C-e> <End>

" Hebrew
noremap <F2> :setlocal invrightleft<CR>
inoremap <F2> <Esc>:setlocal invrightleft<CR>a

" Scrolling of the screen
noremap <C-j> 3<C-e>
noremap <C-k> 3<C-y>

" Disable escape timout in terminal
set ttimeout
set ttimeoutlen=20
set notimeout

" Tags
noremap g] g<C-]>

" Omnicomplete
set completeopt-=preview

" NERDCommenter
let g:NERDSpaceDelims = 1

" Strip all whitespaces
noremap <F12> :%s/\s\+$//<CR>``

" Easier access to netrw
noremap - :E<CR>

" Replace current word
noremap <leader>s :%s/<c-r><c-w>/
noremap <leader>S :%s/<c-r><c-a>/
