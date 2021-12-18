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

set grepprg=ag\ --nogroup\ --nocolor
let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --ignore ''.git'' --hidden -g ""'
let g:ctrlp_use_caching = 0 " Ag is so fast that caching isn’t necessary
let g:ctrlp_max_files = 10000
let g:ctrlp_working_path_mode = 'ra'   " Always use the current working directory rather than the location of the current file
let g:ctrlp_mruf_last_entered = 1

" Markdown
let g:markdown_fenced_languages = ['css', 'javascript', 'js=javascript', 'json=javascript']

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

" Nerdtree
nnoremap <leader>n :NERDTreeFind<CR>
let NERDTreeHijackNetrw = 0
autocmd vimenter * if !argc() | NERDTree | wincmd l | endif
let NERDTreeIgnore=['^components/', '^node_modules/', '^bower_components/', '^dist/']
let NERDTreeShowHidden=1
"nnoremap <leader>x :NERDTreeMapOpenSplit<CR>
"nnoremap <leader>v :NERDTreeMapOpenVSplit<CR>


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
