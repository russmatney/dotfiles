" Indent Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=grey  ctermbg=237
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=239

" Ctrl-P
set wildignore+=node_modules,.tmp,.dist
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|\.git/\|platforms'

" NerdTree
nnoremap <leader>n :NERDTreeToggle<CR>
let NERDTreeHijackNetrw = 0
autocmd vimenter * if !argc() | NERDTree | endif
let NERDTreeIgnore=['^node_modules/', '^.tmp/']

" Markdown
let g:markdown_fenced_languages = ['coffee', 'css', 'javascript', 'js=javascript', 'json=javascript', 'styl=stylus']
