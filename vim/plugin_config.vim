" Indent Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup=1

" Ctrl-P
set wildignore=node_modules,.tmp,.dist

" NerdTree
nnoremap <leader>n :NERDTreeToggle<CR>
let NERDTreeHijackNetrw = 0
autocmd vimenter * if !argc() | NERDTree | windcmd 1 | endif
let NERDTreeIgnore=['^node_modules/', '^.tmp/']

" Markdown
let g:markdown_fenced_languages = ['coffee', 'css', 'javascript', 'js=javascript', 'json=javascript', 'styl=stylus']
