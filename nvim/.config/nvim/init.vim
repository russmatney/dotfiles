set t_Co=256
let g:hybrid_use_Xresources = 1
let $NVIM_TUI_ENABLE_TRUE_COLOR=1


" autocmd VimEnter * color OceanicNext
" autocmd VimEnter * color srcery
" autocmd VimEnter * color hybrid-material
" autocmd VimEnter * color tomorrow
autocmd VimEnter * color tomorrow-night-eighties
set background=dark

let mapleader=" "
"set showcmd
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

inoremap jj <ESC>

"quick goyo
nnoremap <leader>g :Goyo<CR>

"quick splits
nnoremap <leader>v <c-w>v<c-w>l
nnoremap <leader>s <c-w>s<c-w>k

"quick swap
nnoremap <leader><leader> <c-^>

"align
nnoremap <leader>a :Ag

"open file in same dir
nnoremap <leader>E :Explore<CR>

" Conventional Emacs line-editor defaults
" yanked from: @wpcarro
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A

"window resizing
"nnoremap <leader>l <c-w>10<
"nnoremap <leader>h <c-w>10>

"rerun syntax highlighting (helpful for large files)
nmap <leader>f :syntax sync fromstart<CR>
inoremap <C-f> <Esc>:syntax sync fromstart<CR>i

"for those mofoking binaries
"nmap <leader>j :setf javascript<CR>

"Repeat last colon-command @wpcarro
nnoremap ;; @:<CR>
vnoremap ;; @:<CR>

"unhighlight
nmap <leader>h :nohlsearch<CR>

"move split in direction
nmap <leader>H <C-W>H
nmap <leader>L <C-W>L
nmap <leader>K <C-W>K
nmap <leader>J <C-W>J

" // to search for visually selected text
vnoremap // y/<C-R>"<CR>

nmap <leader>r :redraw!<CR>

nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>

nnoremap <Tab> :BufSurfForward<CR>
nnoremap <S-Tab> :BufSurfBack<CR>

nnoremap <leader>= <C-w>=

function! DeleteEmptyBuffers()
    let [i, n; empty] = [1, bufnr('$')]
    while i <= n
        if bufexists(i) && bufname(i) == ''
            call add(empty, i)
        endif
        let i += 1
    endwhile
    if len(empty) > 0
        exe 'bdelete' join(empty)
    endif
endfunction

nmap <leader><CR> :w<CR>

nmap <leader>bd :call DeleteEmptyBuffers()<CR>
nmap <leader>1 :1b<CR>
nmap <leader>2 :2b<CR>
nmap <leader>3 :3b<CR>
nmap <leader>4 :4b<CR>
nmap <leader>5 :5b<CR>
nmap <leader>6 :6b<CR>
nmap <leader>7 :7b<CR>
nmap <leader>8 :8b<CR>
nmap <leader>9 :9b<CR>
nmap <leader>0 :10b<CR>

exec 'set viminfo=%,' . &viminfo

" New file in same dir - ripped from rschmukler
" command -nargs=1 DE :e `dirname %`/<args>

" Dash
nnoremap <leader>d :Dash<CR>

nmap <leader>p <C-p>

lua require('plugins')
source ~/.config/nvim/filetype-settings.vim
source ~/.config/nvim/config.vim
source ~/.config/nvim/tmux_navigator.vim
lua require('bindings')

" Re-source this configuration file
nnoremap gsi :so ~/.config/nvim/init.vim<CR>
