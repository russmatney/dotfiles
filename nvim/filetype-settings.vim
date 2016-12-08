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

" Elm
au BufReadPre * setlocal tabstop=2
au BufReadPre * setlocal shiftwidth=2

au BufReadPre *.elm setlocal tabstop=4
au BufReadPre *.elm setlocal shiftwidth=4


" Elixir
" Run elixir tests on that line. Credit to: @wpcarro
nnoremap <leader>t :call ExTestToggle()<CR>

" Jumps from an Elixir module file to an Elixir test file.
fun! ExTestToggle()
  if expand('%:e') == "ex"

    let test_file_name = expand('%:t:r') . "_test.exs"
    let test_file_dir = substitute(expand('%:p:h'), "/lib/", "/test/", "")
    let full_test_path = join([test_file_dir, test_file_name], "/")

    e `=full_test_path`

  elseif match(expand('%:t'), "_test.exs") != -1

    let test_file_name = expand('%:t:r')
    let offset_amt = strlen(test_file_name) - strlen("_test")
    let module_file_name = strpart(test_file_name, 0, offset_amt) . ".ex"
    let module_file_dir = substitute(expand('%:p:h'), "/test/", "/lib/", "")
    let full_module_path = join([module_file_dir, module_file_name], "/")

    e `=full_module_path`

  endif
endfun
