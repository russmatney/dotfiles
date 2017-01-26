" Indent Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=black ctermbg=237
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=grey ctermbg=239

" Vim Colors
let g:enable_bold_font = 1

" Ctrl-P
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules,lib-cov,public,bower_components,dist,built,typings,doc
set wildignore+=node_modules,.tmp,.dist,.git,platforms,bower_components,jspm,dist,rethinkdb_data,vendor
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|\.git/\|platforms\|bower_components\|jspm\|dist\|rethinkdb_data\|vendor\|doc'

set grepprg=Ag\ --nogroup\ --nocolor
let g:ctrlp_user_command = 'Ag %s -i --nocolor --nogroup --ignore ''.git'' --hidden -g ""'
let g:ctrlp_use_caching = 0 " Ag is so fast that caching isn’t necessary
let g:ctrlp_max_files = 10000
let g:ctrlp_working_path_mode = 'ra'   " Always use the current working directory rather than the location of the current file
let g:ctrlp_mruf_last_entered = 1

" Markdown
let g:markdown_fenced_languages = ['css', 'javascript', 'js=javascript', 'json=javascript']

" Syntastic
" let g:syntastic_always_populate_loc_list = 0
" let g:syntastic_html_checkers = []
" let g:syntastic_javascript_checkers = ['eslint']
" let g:syntastic_go_checkers = ['govet', 'golint', 'errcheck']
" nnoremap <leader>{ :lprev<CR>
" nnoremap <leader>} :lnext<CR>

" NeoMake
autocmd! BufWritePost * Neomake
autocmd! BufWritePost *_test.go Neomake gotest

let g:neomake_go_enabled_makers = ['go', 'govet', 'golint']
let g:neomake_go_go_maker = {
    \ 'exe': 'sh',
    \ 'args': ['-c', 'go build -o /dev/null ./\$0', '%:h'],
    \ 'errorformat':
        \ '%W%f:%l: warning: %m,' .
        \ '%E%f:%l:%c:%m,' .
        \ '%E%f:%l:%m,' .
        \ '%C%\s%\+%m,' .
        \ '%-G#%.%#'
    \ }

let g:neomake_go_golint_maker = {
    \ 'exe': 'golint',
    \ 'cwd': '%:h',
    \ 'errorformat':
        \ '%W%f:%l:%c: %m,' .
        \ '%-G%.%#'
    \ }

let g:neomake_go_gotest_maker = {
    \ 'exe': 'sh',
    \ 'args': ['-c', 'go test ./\$0', '%:h'],
    \ 'errorformat':
        \ '%W%f:%l: warning: %m,' .
        \ '%E%f:%l:%c:%m,' .
        \ '%E%f:%l:%m,' .
        \ '%C%\s%\+%m,' .
        \ '%-G#%.%#'
    \ }

let g:neomake_typescript_tsc_maker = {
  \ 'args': [ '--noEmit' ],
  \ 'append_file': 0,
  \ 'errorformat':
  \ '%E%f %#(%l\,%c): error %m,' .
  \ '%E%f %#(%l\,%c): %m,' .
  \ '%Eerror %m,' .
  \ '%C%\s%\+%m'
\ }

let g:neomake_typescript_enabled_makers = ['tsc', 'tslint']
let g:neomake_open_list = 0

let g:neomake_rust_cargo_maker = {
      \ 'exe': 'cargo',
      \ 'args': ['rustc', '--', '-Z', 'no-trans' ],
      \ 'append_file': 0,
      \ 'errorformat':
      \   '%E%f:%l:%c: %\d%#:%\d%# %.%\{-}error:%.%\{-} %m,'   .
      \   '%E%f:%l:%c: %\d%#:%\d%# %.%\{-}error: %m,'   .
      \   '%W%f:%l:%c: %\d%#:%\d%# %.%\{-}warning:%.%\{-} %m,' .
      \   '%W%f:%l:%c: %\d%#:%\d%# %.%\{-}warning: %m,' .
      \   '%Z%f:%l %m,' .
      \   '%C%f:%l %m,' .
      \   '%C   %m,' .
      \   '%C%m,' .
      \   '%-Z%.%#'
      \ }

if filereadable("Cargo.toml")
  let g:neomake_rust_enabled_makers = ['cargo']
else
  let g:neomake_rust_enabled_makers = ['rustc']
endif

let g:neomake_javascript_enabled_makers = ['eslint']

" `mixy` courtesy of github.com/rschmukler/mixy
let g:neomake_elixir_mix_maker = {
      \ 'exe': 'mixy',
      \ 'args': ['compile'],
      \ 'append_file': 0,
      \ 'errorformat':
      \   '%Wwarning: %m,' .
      \   '%C%f:%l%.%#,' .
      \   '%E** (%s) %f:%l: %m,' .
      \   '%C%.%#,' .
      \   '%E** (%s) %m,' .
      \   '%C%.%#,' .
      \   '%C%f:%l%.%#,' .
      \   '%-Z%.%#'
      \ }

let g:neomake_elixir_credo_maker = {
      \ 'exe': 'mix',
      \ 'args': ['credo', 'list', '%:p', '--format=oneline'],
      \ 'errorformat':
      \   '%W[F] %. %f:%l:%c %m,' .
      \   '%W[F] %. %f:%l %m,' .
      \   '%W[R] %. %f:%l:%c %m,' .
      \   '%W[R] %. %f:%l %m,' .
      \   '%I[C] %. %f:%l:%c %m,' .
      \   '%I[C] %. %f:%l %m,' .
      \   '%-Z%.%#'
      \ }


let g:neomake_elixir_enabled_makers = ['mix', 'credo']

augroup my_error_signs
  au!
  autocmd ColorScheme * hi NeomakeErrorSign ctermfg=203 guifg=#ff5f5f
  autocmd ColorScheme * hi NeomakeWarningSign ctermfg=209 guifg=#ffaf00
  autocmd ColorScheme * hi NeomakeInfoSign ctermfg=183 guifg=#dfafff
  autocmd ColorScheme * hi NeomakeMessageSign ctermfg=27 guifg=#0087ff
augroup END


let g:neomake_error_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeErrorSign',
            \ }

let g:neomake_warning_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeWarningSign',
            \ }

let g:neomake_info_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeInfoSign',
            \ }

let g:neomake_message_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeMessageSign',
            \ }

function! <SID>LocationPrevious()
  try
    lprev
  catch /^Vim\%((\a\+)\)\=:E553/
    llast
  endtry
endfunction

function! <SID>LocationNext()
  try
    lnext
  catch /^Vim\%((\a\+)\)\=:E553/
    lfirst
  endtry
endfunction

nnoremap <Leader>[ :call <SID>LocationPrevious()<CR>
nnoremap <Leader>] :call <SID>LocationNext()<CR>

" Tsuquyomi
autocmd FileType typescript setlocal completeopt+=menu,preview
autocmd FileType typescript nmap <buffer> <Space>t : <C-u>echo tsuquyomi#hint()<CR>
let g:tsuquyomi_disable_quickfix = 1
nnoremap <Leader>d :TsuDefinition<CR>
nnoremap <Leader>D :TsuGoBack<CR>

"js-template highlighting
autocmd FileType typescript nnoremap <leader>T :JsPreTmpl html<CR>
autocmd FileType javascript nnoremap <leader>T :JsPreTmpl html<CR>

" Tern
let g:tern_map_keys=1

" Supertab
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
let g:SuperTabClosePreviewOnPopupClose = 1
autocmd FileType go setlocal completeopt-=preview

" Goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:goyo_width = 200



"Nerdtree
nnoremap <leader>n :NERDTreeFind<CR>
let NERDTreeHijackNetrw = 0
autocmd vimenter * if !argc() | NERDTree | wincmd l | endif
let NERDTreeIgnore=['^components/', '^node_modules/', '^bower_components/', '^dist/']
"nnoremap <leader>x :NERDTreeMapOpenSplit<CR>
"nnoremap <leader>v :NERDTreeMapOpenVSplit<CR>

"Rust and Vim Racer
let g:racer_cmd = "~/.multirust/toolchains/beta/cargo/bin/racer"
let $RUST_SRC_PATH="/usr/local/src/rust/beta"
au FileType rust nmap <Leader>d :call RacerGoToDefinition()<CR>
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1

au FileType rust command! Nofmt set paste | normal O#[cfg_attr(rustfmt, rustfmt_skip)]<ESC>:set nopaste<CR>^j
au FileType rust nmap <Leader>i :Nofmt<CR>
au FileType rust nmap <Leader>r :RustRun<CR>

"Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#ignore_sources={}

"" GOLANG
" vim-go
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <leader>d <Plug>(go-doc)
au FileType go nmap <leader>l <Plug>(go-def)
au FileType go nmap <leader>i :GoImports<CR>
let g:go_highlight_interfaces = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_fail_silently = 1
" let g:go_auto_type_info = 1
let g:go_gocode_autobuild = 1
let g:go_gocode_propose_builtins = 1

nmap <leader>o :TagbarToggle<CR>

" Elixir
let g:deoplete#ignore_sources.elixir=['member']
let g:elixir_autobuild=1
let g:elixir_showerror=0
au FileType elixir nmap <Leader>d :ExDoc<CR>
au FileType elixir nmap <Leader>l :ExDef<CR>
" au FileType elixir nmap <Leader>r :ElixirExec<CR>
let g:alchemist#elixir_erlang_src = "/usr/local/share/src"

" Elm
au FileType elm nmap <leader>d :ElmShowDocs<CR>
autocmd! BufWritePost *.elm silent exec "!ctags"
let g:elm_detailed_complete = 1
let g:ycm_semantic_triggers = {
     \ 'elm' : ['.'],
     \}


" Powerline
let g:Powerline_symbols = 'fancy'
" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline_theme='oceanicnext'
let g:airline_section_b = ''
let g:airline_section_x = ''
let g:airline_section_y = '%t'
let g:airline_section_z = ''

let g:airline_section_b = ''
let g:airline_section_x = '%t'
let g:airline_section_y = ''
let g:airline_section_z = ''



" fzf.vim
let $FZF_DEFAULT_COMMAND = 'Ag -g ""'
