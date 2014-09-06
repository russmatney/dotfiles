# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="jbergantine"

# disable command autocorrection
DISABLE_CORRECTION="true"

# red dots displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# prevent zsh from renaming the window
DISABLE_AUTO_TITLE=true

# oh-my-zsh plugins
plugins=(git brew heroku osx rvm)

source $ZSH/oh-my-zsh.sh

#nvm source and bash_completions
source ~/.nvm/nvm.sh
[[ -r $NVM_DIR/bash_completion ]] && . $NVM_DIR/bash_completion
export PATH=./node_modules/.bin:$PATH

#rvm source
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

#golang
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin
export GOPATH=$HOME/projects/go-projects
export PATH=$PATH:$GOPATH/bin
export PATH=$HOME/bin:/usr/local/bin:$PATH

#goenv
export PATH="$HOME/.goenv/bin:$PATH"
eval "$(goenv init -)"

# ssh
export SSH_KEY_PATH="~/.ssh/dsa_id"


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

alias 'amnesia'='update-index --assume-unchanged'
alias 'recall'='update-index --no-assume-unchanged'
alias 'forgetfulness'="!sh -c 'git ls-files -v | grep ~[a-z]'"

alias 'letsgo'='cd `go env GOPATH`/src/github.com'

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

#TMUX aliases (thanks to/ripped from @rschmukler)
tmux_search_paths=( )

function tt() {
  if ! tmux has-session -t "$1" 2> ~/projects/null; then
    tmux_script=~/dotfiles/files/tmux-scripts/$1
    if [[ -e $tmux_script ]]; then
      zsh "$tmux_script"
    else
      oldTMUX=$TMUX
      unset TMUX
      tmux new -d -s $1
      export TMUX=$oldTMUX
      unset oldTMUX
      for searches in $tmux_search_paths; do
        dir=$searches/$1
        if [[ -d $dir ]]; then
          tmux send-keys -t "${1}" "cd $dir; clear" "C-m"
          break
        fi
      done
      unset searches
      unset tmux_scripts
      unset dir
    fi
  fi
  if [[ -n $TMUX ]]; then
    tmux switch-client -t $1
  else
    tmux attach -t $1
  fi
}

# gather files for auto-complete
function _tls() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
}
function _tscripts() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
  reply+=( $(ls ~/dotfiles/files/tmux-scripts) )
  for dir in $tmux_search_paths; do
    reply+=( $(ls $dir/*/) )
  done
}

#handy functions
function tn() {
  tmux new -s "$1"
}
function tk() {
  tmux kill-session -t $1
}
function tm() {
  tmux new-session -t $1
}
function ta() {
  tmux attach -t "$1"
}

# autocompletion attached to functions
compctl -K _tls ta
compctl -K _tls tk
compctl -K _tls tm
compctl -K _tscripts tt

alias tls="tmux list-sessions";
