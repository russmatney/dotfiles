# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="moveline"

# disable command autocorrection
DISABLE_CORRECTION="true"

# red dots displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# oh-my-zsh plugins
plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=$HOME/bin:/usr/local/bin:$PATH

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"


# Lines configured by zsh-newuser-install
setopt appendhistory beep nomatch notify
unsetopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install


# Git Aliases
alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

