" JSON File Settings
au BufNewFile,BufRead *.json set ft=javascript

" Markdown File Settings
autocmd BufReadPre *.md setlocal textwidth=80

" Text File Settings
"autocmd BufReadPre *.txt setlocal textwidth=80

autocmd BufReadPre *.ejs set ft=html
autocmd BufReadPre *.less set ft=css

" Ruby
autocmd BufReadPre Gemfile set ft=ruby
autocmd BufReadPre Gemfile.lock set ft=ruby
autocmd BufReadPre *.ru set ft=ruby

" PHP
autocmd FileType php setlocal colorcolumn=100 noexpandtab tabstop=2 shiftwidth=2
autocmd FileType php setlocal errorformat=%m\ in\ %f\ on\ line\ %l
autocmd FileType php setlocal noeol binary fileformats="mac,unix,dos"

