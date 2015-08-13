##Workflow from scratch notes:

download chrome from safari

sign into chrome to resync

clear/keep chrome extensions

  - pocket, one password, what font, vimium

install 1password app

install dropbox

install iTerm

  turn off lion style full screen windows

install alfred

install dash

  add dash-alfred integration

install nvalt

  set up to read plain text files from dropbox

install limechat

install betterSnapTool

install xcode

  install command line tools

evernote

slack

colorsnapper2 

install homebrew

  install neovim

start dotfiles
  alias vim=nvim
  install antigen as submodule
  build basic antigen plugins listbrew 
     brew, git, nvm, a theme, apply it
set iterm colors to something else
   (install color schemes?)

brew install tmux

  tmux.conf

    lots of lil things

nvim

  nvimrc

  install vim-plug

  write vim-plugins.vim

osx-hacks script

  fast repeat is a game-changer 

  shut down + restart required.

zshrc build out

  brew install coreutils

  tmux aliases, zsh aliases

nvimrc build out

  plugin-config

brew install node

  npm install -g n

  n io latest

  npm i -g bower gulp eslint

brew install Ag

get pip?

sudo easy_install pip

install term properly

cd ~/.vim/plugged/tern_for_vim; npm i;

sudo pip install neovim

####to fix neovim ctrl + h break

infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti ; tic $TERM.ti


TODO: mjolnir, custom zsh theme, vim colorscheme, powerline, iterm colors


brew install caskroom/cask/brew-cask

brew cask install qlmarkdown

npm i -g babel

npm i -g eslint

brew install reattach-to-user-namespace

brew install lua

echo 'rocks_servers = { "http://rocks.moonscript.org" }' >> /usr/local/etc/luarocks52/config-5.2.lua

luarocks install mjolnir.hotkey

luarocks install mjolnir.application

brew cask install mjolnir


download https://pqrs.org/osx/karabiner/seil.html for caps lock -> esc remapping

brew install rethinkdb

brew install cassandra

npm i -g typescript

:VimProcInstall

brew install httpie
