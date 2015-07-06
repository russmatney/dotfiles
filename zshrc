source ~/dotfiles/zsh/antigen-plugins.zsh

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH

alias vim='nvim'
alias ls='gls --color=always'

COMPLETION_WAITING_DOTS=true
DISABLE_AUTO_TITLE=true

# Git Aliases
alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias 'gpb'="git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d"
alias 'gprom'="git pull --rebase origin master"

source ~/dotfiles/zsh/tmux-aliases.zsh

