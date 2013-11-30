set hlsearch

function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction

call MapCR()

augroup mapcr
    autocmd!
    " Unmap CR in command window
    autocmd! CmdwinEnter * unmap <cr>
    autocmd! CmdwinLeave * call MapCR()
augroup END
