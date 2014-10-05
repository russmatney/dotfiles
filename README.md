dotfiles
========

Zsh with oh-my-zsh, tmux, vim with plugins via vundle, and an install.rb script

##Installation

Clone into ~/dotfiles and run `ruby install.rb`.
This will setup symlinks for your files and install vim plugins.

##zshrc
###oh-my-zsh
##vimrc

I love me some vim!

I use vundle to handle plugins, and the install.rb will automagically pull + install any repos you add to the vundle file.

##vim/vundle.vim

Vim at it's core is great, but when combined with the right plugins,
the code writes itself.

Syntax plugins are a must.

Other notables:

- [vim-gitgutter](https://github.com/airblade/vim-gitgutter) - shows a gutter to expose the git diff as you write
- [vim-surround](https://github.com/tpope/vim-surround) - gives the quick surround ability I loved in sublime. `c - s
  - w - '` will wrap the current word in `''`, etc.
- [ctrlp](https://github.com/kien/ctrlp.vim) - quick file search and auto-complete. A MUST.
- [vim-indent-guides](https://github.com/nathanaelkane/vim-indent-guides) - some helpful guidelines for whatever language. You won't
  regret adding them.
- [ag.vim](https://github.com/rking/ag.vim) - the silver searcher is the best way to search for anything in your
  codebase, within or outside of vim.

##tmux.conf

Tmux handles windows and panes within iTerm, and with a few conveniences, can make for a very efficient workflow.

###files/tmux-scripts

Tmux scripts are a great way to quickly leap between common workflows. I like to
separate front-end and back-end apps, and often larger apps need one-off apps to
support whatever stuff. These scripts let you leap between flows with no
overhead.

##install.rb
##gitconfig

Some useful aliases here, courtesy of @cyranix.

##TODO:

- Baby stepping blog post
- overview of each file/what it does/how it's used
- usage for the install.rb
