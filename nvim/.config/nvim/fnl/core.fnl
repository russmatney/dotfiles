(module core)

(local a (require :aniseed.core))
(local nvim (require :aniseed.nvim))
(a.println "evaling core.fnl")

(fn keymap [...]
  (vim.api.nvim_set_keymap ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-negotiable bindings

(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

; reload everything (source this file)
(keymap "n" "gr" ":so ~/.config/nvim/init.lua<CR>" {:noremap true})

; file save
(keymap "n" "<leader><CR>" ":w<CR>" {:noremap true})

; toggle file
(keymap "n" "<leader><leader>" "<c-^>" {:noremap true})

; quick dir nav
(keymap "n" "-" ":Explore<CR>" {:noremap true})

; unhighlight
(keymap "n" "<CR>" ":noh<CR><CR>" {:noremap true})

; splits
(keymap "n" "<leader>v" "<c-w>v<c-w>" {:noremap true})
(keymap "n" "<leader>s" "<c-w>s<c-w>k" {:noremap true})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc config

; set t_Co=256
; let g:hybrid_use_Xresources = 1
; let $NVIM_TUI_ENABLE_TRUE_COLOR=1

(vim.cmd "
autocmd VimEnter * color Tomorrow-Night-Eighties
set background=dark

set showcmd
set hidden

set completeopt=longest,menuone,preview

set shell=zsh

set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

set encoding=utf-8
set timeoutlen=300

set nowrap
set number

set backspace=2

set clipboard=unnamedplus

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
")

(vim.cmd "
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

exec 'set viminfo=%,' . &viminfo

let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=black ctermbg=237
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=grey ctermbg=239

let g:enable_bold_font = 1
")

; Markdown
(vim.cmd "
let g:markdown_fenced_languages = ['css', 'javascript', 'js=javascript', 'json=javascript']
")

; Powerline, Airline
(vim.cmd "
let g:Powerline_symbols = 'fancy'

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
let g:airline_section_z = ''")

; trigger `autoread` when files changes on disk
; notification after file change
(vim.cmd "
set autoread
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif
autocmd FileChangedShellPost * echohl WarningMsg | echo \"File changed on disk. Buffer reloaded.\" | echohl None
")

(vim.cmd "
au BufNewFile,BufRead *.json set ft=javascript
autocmd BufReadPre *.md setlocal textwidth=80
autocmd BufReadPre *.txt setlocal textwidth=80
autocmd BufReadPre *.ejs set ft=html
autocmd BufReadPre *.less set ft=css
autocmd FileType lua nnoremap <buffer> <c-k> :call LuaFormat()<cr>
autocmd BufWrite *.lua call LuaFormat()
")

(fn setup []
  (: (require "bindings") :setup-keybindings)
)

{: setup}

