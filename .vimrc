" Load pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" Make Vim more useful
set nocompatible

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

set guifont=Courier:h13
colorscheme badwolf

" Hebrew
noremap <F2> :setlocal invrightleft<CR>
inoremap <F2> <Esc>:setlocal invrightleft<CR>a

" Scrolling of the screen
noremap <C-j> 3<C-e>
noremap <C-k> 3<C-y>

" Strip all whitespaces
function! StripWhitespace()
	let save_cursor = getpos(".")
	let old_query = getreg('/')
	:%s/\s\+$//e
	call setpos('.', save_cursor)
	call setreg('/', old_query)
endfunction
noremap <leader>ss :call StripWhitespace()<CR>

