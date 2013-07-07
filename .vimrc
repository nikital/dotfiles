" Load pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" Make Vim more useful
set nocompatible

" Load cool plugins
filetype plugin indent on

" Syntax highlighting
syntax on

" Allow unsaved background buffers
set hidden

" Temporary stuff
if !isdirectory("/tmp/.vimtmp")
	call mkdir("/tmp/.vimtmp")
endif
if !isdirectory("/tmp/.vimtmp/swap")
	call mkdir("/tmp/.vimtmp/swap")
endif
if !isdirectory("/tmp/.vimtmp/backup")
	call mkdir("/tmp/.vimtmp/backup")
endif
set backup
set writebackup
set directory=/tmp/.vimtmp/swap
set backupdir=/tmp/.vimtmp/backup

" Tabbing
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
set smartindent
set cindent

" Searching
set incsearch
set nohlsearch
set ignorecase smartcase

" Theming
set cursorline
set laststatus=2
set ruler
set number
set showcmd
set guifont=Menlo:h13
colorscheme molokai

" Editing
set backspace=indent,eol,start
inoremap <C-e> <End>

" Hebrew
noremap <F2> :setlocal invrightleft<CR>
inoremap <F2> <Esc>:setlocal invrightleft<CR>a

" Scrolling of the screen
noremap <C-j> 3<C-e>
noremap <C-k> 3<C-y>

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
