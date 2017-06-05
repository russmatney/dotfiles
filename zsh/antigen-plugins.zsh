source ~/dotfiles/antigen/antigen.zsh

antigen use oh-my-zsh

antigen theme https://gist.github.com/rschmukler/7d9317cde82c58be965d rs2.zsh-theme

antigen bundle zsh-users/zsh-syntax-highlighting

antigen bundle Tarrasch/zsh-autoenv
antigen bundle yerinle/zsh-gvm

antigen bundle brew
antigen bundle osx
antigen bundle nvm
antigen bundle rvm
antigen bundle docker
antigen bundle git
antigen bundle bower
antigen bundle sudo
antigen bundle emacs

antigen-apply
