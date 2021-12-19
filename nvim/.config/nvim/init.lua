-- TODO handle this chicken and egg
require("impatient")

-- Enable Aniseed's automatic compilation and loading of Fennel source code.
-- TODO safe compile (don't panic on compilation errors)
require("aniseed.env").init({module = "core", compile = true})

-- ensure packages and their configuration
local packages = require("packages")
packages.setup_packages()

-- require fennel core, which should be compiled above by aniseed#env
print("requiring core")
local core = require("core")
core.setup_keybindings()

vim.cmd [[
" config.vim

" colors
set t_Co=256
let g:hybrid_use_Xresources = 1
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" theme
autocmd VimEnter * color Tomorrow-Night-Eighties
set background=dark

set showcmd
set hidden

set completeopt=longest,menuone,preview

autocmd BufWritePre * %s/\s\+$//e

set shell=zsh

set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

set encoding=utf-8
set timeoutlen=300

set nowrap
set number

set backspace=2

set clipboard=unnamedplus

" indents and tab stuff
set smartindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set shiftround
set smarttab
set autoindent
set copyindent
set expandtab

set wildmode=longest,list,full

set ignorecase
set smartcase
set hlsearch
set incsearch

set history=1000
set undolevels=1000

syntax on
syntax enable
filetype plugin indent on

"force markdown syntax
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

exec 'set viminfo=%,' . &viminfo

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

" trigger `autoread` when files changes on disk
set autoread
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
" notification after file change
autocmd FileChangedShellPost *
  \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None

" JSON File Settings
au BufNewFile,BufRead *.json set ft=javascript

" Markdown File Settings
autocmd BufReadPre *.md setlocal textwidth=80

" Text File Settings
"autocmd BufReadPre *.txt setlocal textwidth=80

autocmd BufReadPre *.ejs set ft=html
autocmd BufReadPre *.less set ft=css

" Lua
autocmd FileType lua nnoremap <buffer> <c-k> :call LuaFormat()<cr>
autocmd BufWrite *.lua call LuaFormat()

]]
