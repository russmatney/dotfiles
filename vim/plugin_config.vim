" Ctrl-P

set wildignore=node_modules,.tmp,.dist

" NerdTree
nnoremap <leader>n :NERDTreeToggle<CR>
let NERDTreeHijackNetrw = 0
autocmd vimenter * if !argc() | NERDTree | windcmd 1 | endif
let NERDTreeIgnore=['^node_modules/', '^.tmp/']

" Markdown
let g:markdown_fenced_languages = ['coffee', 'css', 'javascript', 'js=javascript', 'json=javascript', 'styl=stylus']
