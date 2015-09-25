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


Truecolor in iterm2/tmux/vim:

- install [nightly iterm](https://iterm2.com/downloads/nightly/#/section/home)
- [patch tmux](https://github.com/rschmukler/dotfiles/blob/master/files/brew-patches/tmux.txt) and brew reinstall --with-truecolor
- install a [true color theme](https://github.com/kristijanhusak/vim-hybrid-material), plus some vim related things


Airline installed, powerline installed and [powerline
fonts](https://github.com/powerline/fonts) downloaded installed
via script. font chosen via iTerm2 profile settings. ([Hack](https://github.com/powerline/fonts/tree/master/Hack), (or [patched Hack for powerline](https://github.com/chrissimpkins/Hack/issues/33)))

Alfred Workflows:

- [pkgman](https://github.com/willfarrell/alfred-pkgman-workflow) - better/faster than googling for packages
- [dash](https://github.com/Kapeli/Dash-Alfred-Workflow) - better/faster than googling for docs
- [spotifious](http://ben.stolovitz.com/Spotify-for-Alfred/) - better/faster search/control of spotify

- [workflow search](https://github.com/hzlzh/Alfred-Workflows/raw/master/Downloads/Workflow-Searcher.alfredworkflow) - for when you know there's a better/faster way
- [stackoverflow](https://github.com/xhinking/Alfred/blob/master/stackoverflow.alfredworkflow)
- [github](https://github.com/gharlan/alfred-github-workflow)
- [top (including kill)](http://zhaocai.github.io/alfred2-top-workflow/) - because the activity monitor is just too slow, especially when something needs to DIE
- [ip](http://dferg.us/ip-address-workflow/) - quick local/external ip lookup


brew install python
pip install powerline-status
pip install psutil
