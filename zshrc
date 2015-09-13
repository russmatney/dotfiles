source ~/dotfiles/zsh/antigen-plugins.zsh

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH

alias vim='NVIM_TUI_ENABLE_TRUE_COLOR=true nvim'
alias vi='NVIM_TUI_ENABLE_TRUE_COLOR=true nvim'
alias ls='gls --color=always'

COMPLETION_WAITING_DOTS=true
DISABLE_AUTO_TITLE=true

# Git Aliases
alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias 'gpb'="git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d"
alias 'gprom'="git pull --rebase origin master"

source ~/dotfiles/zsh/tmux-aliases.zsh

alias pg_start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pg_stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

alias dm='docker-machine'
alias dc='docker-compose'

function dme() {
  eval "$(docker-machine env $1)"
}

function drm() {
  docker stop "$1";
  docker rm -v "$1";
}

alias dps='docker ps -a'
alias dmls='docker-machine ls'
alias dip='docker-machine ip'

test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh
