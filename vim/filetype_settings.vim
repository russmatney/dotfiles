" JSON File Settings
au BufNewFile,BufRead *.json set ft=javascript

" Markdown File Settings
autocmd BufReadPre *.md setlocal textwidth=80
autocmd BufReadPre *.md setlocal spell spelllang=en_us

" Text File Settings
autocmd BufReadPre *.txt setlocal textwidth=80
autocmd BufReadPre *.txt setlocal spell spelllang=en_us
