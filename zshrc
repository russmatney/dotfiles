# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="moveline"

# disable command autocorrection
DISABLE_CORRECTION="true"

# red dots displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# prevent zsh from renaming the window
DISABLE_AUTO_TITLE=true

# oh-my-zsh plugins
plugins=(git)

source $ZSH/oh-my-zsh.sh

#nvm source and bash_completions
source ~/.nvm/nvm.sh
[[ -r $NVM_DIR/bash_completion ]] && . $NVM_DIR/bash_completion
export PATH=./node_modules/.bin:$PATH

#rbenv source
eval "$(rbenv init -)"

#golang
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin
export GOPATH=$HOME/projects/go-projects
export PATH=$HOME/bin:/usr/local/bin:$PATH

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"


# Lines configured by zsh-newuser-install
setopt appendhistory beep nomatch notify
unsetopt autocd

# enables vim keybindings on shell commands
bindkey -v
# End of lines configured by zsh-newuser-install


# Git Aliases
alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

alias cgrep="grep --color=auto"
alias :q="exit"
alias :wq="exit"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
