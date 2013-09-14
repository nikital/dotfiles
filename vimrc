" Clear all autocommands
autocmd!

" Load pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" Make Vim more useful
set nocompatible

" Load filetype plugins
filetype plugin indent on

" Syntax highlighting
syntax on

" Allow unsaved background buffers
set hidden

" Temporary files stuff
set nobackup
set writebackup
set noswapfile

" Terminal stuff
set mouse=a
set ttymouse=xterm2 " Makes mouse work under tmux

" Tabbing
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent

" Editing
set backspace=indent,eol,start
inoremap <c-e> <end>
inoremap <c-u> <c-g>u<c-u>
set formatoptions+=j

" Windows
set splitright
set noequalalways

" Searching
set incsearch
set hlsearch
set ignorecase smartcase
function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction
call MapCR()

" Movement
" Remember long moves to jumplist
nnoremap <silent> k :<C-U>execute 'normal!' (v:count > 1 ? "m'" . v:count : '') . 'k'<CR>
nnoremap <silent> j :<C-U>execute 'normal!' (v:count > 1 ? "m'" . v:count : '') . 'j'<CR>

" Theming
set cursorline
set laststatus=2
set ruler
set relativenumber
set number
set showcmd

" Status line
set statusline=
set statusline+=%<\ 
set statusline+=%2*[%n%H%R%W]%*\ 
set statusline+=%f\ %m\ 
set statusline+=%=%1*[
set statusline+=%{strlen(&ft)?&ft:'none'},\ 
set statusline+=%{strlen(&fenc)?&fenc:&enc},\ 
set statusline+=%{&fileformat}
set statusline+=]%*%*\ 
set statusline+=%10((%l/%L,%c)%)\ 
set statusline+=%P\ 

" Color scheme
let g:rehash256 = 1
colorscheme molokai
set background=dark

" Ex-mode completion
set wildmode=longest:full,full
set wildmenu

" Hebrew/RTL
noremap <F2> :setlocal invrightleft<cr>
inoremap <F2> <esc>:setlocal invrightleft<cr>a

" Scrolling of the screen
noremap <c-j> 3<c-e>
noremap <c-k> 3<c-y>

" Disable escape timout in terminal
set ttimeout
set ttimeoutlen=20
set notimeout

" Tags
noremap g] g<c-]>

" Omnicomplete
set completeopt-=preview

" AutoPairs config
let g:AutoPairs = {'{':'}'}

" Easier access to tagbar
noremap <F8> :TagbarToggle<cr>

" Easier access to netrw
noremap - :E<cr>

" Replace current word
noremap <leader>s :%s/<c-r><c-w>/
noremap <leader>S :%s/<c-r><c-a>/

" System clipboard stuff
map <leader>y "+y
map <leader>Y "+Y
map <leader>p "+p
map <leader>P "+P

"""""""""""""""""""""""""""""""
" Autocommands
"""""""""""""""""""""""""""""""
augroup vimrc
    " Clear all commands in the group
    autocmd!

    autocmd FileType text,markdown setlocal textwidth=78
    autocmd FileType help setlocal number relativenumber

    " Recognize scons
    autocmd BufRead,BufNewFile SCons{cript,truct} setfiletype python

    " Unmap CR in command window
    autocmd! CmdwinEnter * :unmap <cr>
    autocmd! CmdwinLeave * :call MapCR()

    autocmd BufWritePost $MYVIMRC source %
augroup END

" Edit files in current directory
cnoremap %% <c-r>=expand('%:h').'/'<cr>
map <leader>e :edit %%

" Rename file
function! RenameFile(new_name)
let old_name = expand('%')
if a:new_name != '' && a:new_name != old_name
    exec ':saveas ' . a:new_name
    exec ':silent !rm ' . old_name
    redraw!
endif
endfunction
command! -complete=file -nargs=1 Rename call RenameFile(<q-args>)
