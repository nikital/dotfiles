let g:neocomplete#enable_at_startup = 1 
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#enable_refresh_always = 1

" Complete from all buffers
if !exists('g:neocomplete#same_filetypes')
    let g:neocomplete#same_filetypes = {}
endif
let g:neocomplete#same_filetypes._ = "_"

inoremap <expr><tab>  pumvisible() ? "\<c-n>" : "\<tab>"
" imap <expr><tab> neosnippet#expandable_or_jumpable() ? "\<plug>(neosnippet_expand_or_jump)" : pumvisible() ? "\<c-n>" : "\<tab>"
" imap <c-k>     <plug>(neosnippet_expand_or_jump)
" smap <c-k>     <plug>(neosnippet_expand_or_jump)
