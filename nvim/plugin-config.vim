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
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules,lib-cov,public,bower_components,dist,built,typings
set wildignore+=node_modules,.tmp,.dist,.git,platforms,bower_components,jspm,dist,rethinkdb_data
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|\.git/\|platforms\|bower_components\|jspm\|dist\|rethinkdb_data'

set grepprg=Ag\ --nogroup\ --nocolor
let g:ctrlp_user_command = 'Ag %s -i --nocolor --nogroup --ignore ''.git'' --hidden -g ""'
let g:ctrlp_use_caching = 0 " Ag is so fast that caching isn’t necessary
let g:ctrlp_max_files = 10000
let g:ctrlp_working_path_mode = 'ra'   " Always use the current working directory rather than the location of the current file

" Markdown
let g:markdown_fenced_languages = ['css', 'javascript', 'js=javascript', 'json=javascript']

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_html_checkers = []
let g:syntastic_javascript_checkers = ['eslint']
nnoremap <leader>{ :lprev<CR>
nnoremap <leader>} :lnext<CR>

" NeoMake
autocmd! BufWritePost * Neomake
let g:neomake_typescript_tsc_maker = {
  \ 'args': [
  \ '--noEmit', '-t', 'ES5', '--module', 'commonjs', '--experimentalDecorators'
  \ ],
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


let g:neomake_error_sign = {
             \ 'text': '>>',
             \ 'texthl': 'ErrorMsg',
             \ }
hi MyWarningMsg ctermbg=3 ctermfg=0
let g:neomake_warning_sign = {
            \ 'text': '>>',
            \ 'texthl': 'Question',
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

" Goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:goyo_width = 200


" Airline
let g:airline_powerline_fonts = 1
" Powerline
let g:Powerline_symbols = 'fancy'
let g:airline_theme='luna'


"Nerdtree
nnoremap <leader>n :NERDTreeToggle<CR>
let NERDTreeHijackNetrw = 0
autocmd vimenter * if !argc() | NERDTree | wincmd l | endif
let NERDTreeIgnore=['^components/', '^node_modules/', '^bower_components/', '^dist/']
"nnoremap <leader>x :NERDTreeMapOpenSplit<CR>
"nnoremap <leader>v :NERDTreeMapOpenVSplit<CR>

set completeopt=longest,menuone,preview

"Rust and Vim Racer
let g:racer_cmd = "~/.multirust/toolchains/beta/cargo/bin/racer"
let $RUST_SRC_PATH="/usr/local/src/rust/beta"
au FileType rust nmap <Leader>d :call RacerGoToDefinition()<CR>
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1

au FileType rust command! Nofmt set paste | normal O#[cfg_attr(rustfmt, rustfmt_skip)]<ESC>:set nopaste<CR>^j
au FileType rust nmap <Leader>i :Nofmt<CR>
au FileType rust nmap <Leader>r :RustRun<CR>

" vim-go
au FileType go nmap <Leader>d :GoDoc<CR>
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

