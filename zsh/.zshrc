#!/usr/bin/env bash

################################################################################
# Environmental Vars
################################################################################

# export EDITOR='emacsclient -nw'
# export VISUAL='emacsclient -nw'
export EDITOR='nvim'
export VISUAL='nvim'
export PAGER='less'

export RANGER_LOAD_DEFAULT_RC=FALSE

export FONTCONFIG_PATH='/etc/fonts'

alias 'zz'='source ~/.zshrc'

################################################################################
# Antibody setup
################################################################################

# From oh-my-zsh
export ZSH="$(antibody home)/https-COLON--SLASH--SLASH-github.com-SLASH-robbyrussell-SLASH-oh-my-zsh"
DISABLE_AUTO_UPDATE="true"

source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt
# source ~/.zsh_plugins.sh

alias 'ra'='antibody bundle \
            < ~/.zsh_plugins.txt \
            > ~/.zsh_plugins.sh && \
            antibody update'


# enable completion features
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i

# allow .hidden file tab completion
setopt globdots


# required to find some shared libs
export LD_LIBRARY_PATH=/usr/local/lib

################################################################################
# Theme/Style
################################################################################

# Restore pywal colors
# ~/.nix-profile/bin/wal -Rq
alias wa='wal -Req'


# Powerline
# powerline-daemon -q
# . /usr/lib/python3.7/site-packages/powerline/bindings/zsh/powerline.zsh

################################################################################
# vim-mode
################################################################################

export KEYTIMEOUT=1

to-tmux-copy-mode() {
  tmux copy-mode
  tmux send-keys "k"
}
autoload to-tmux-copy-mode
zle -N to-tmux-copy-mode

ctrl-r() {
  tmux send-keys "i"
  tmux send-keys "C-r"
}
autoload ctrl-r
zle -N ctrl-r

to-insert-mode() {
  tmux send-keys "i"
}
autoload to-insert-mode
zle -N to-insert-mode

page-up() {
  tmux copy-mode
  tmux send-keys "C-u"
}
autoload page-up
zle -N page-up

search-page() {
  tmux copy-mode
  tmux send-keys "/"
}
autoload search-page
zle -N search-page

search-page-up() {
  tmux copy-mode
  tmux send-keys "?"
}
autoload search-page-up
zle -N search-page-up

paste() {
  tmux send-keys "i"
  tmux send-keys "C-v"
}
autoload paste
zle -N paste

bindkey -M vicmd 'k' to-tmux-copy-mode \
                 'q' to-insert-mode \
                 '\e' to-insert-mode \
                 '^r' ctrl-r \
                 '/' search-page \
                 '?' search-page-up \
                 'p' paste


################################################################################
# Gcloud
################################################################################

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/russ/google-cloud-sdk/path.zsh.inc' ]; then
    source '/Users/russ/google-cloud-sdk/path.zsh.inc';
fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/russ/google-cloud-sdk/completion.zsh.inc' ]; then
    source '/Users/russ/google-cloud-sdk/completion.zsh.inc';
fi


################################################################################
# Fzf
################################################################################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


################################################################################
# Nix
################################################################################
# echo "sourcing nix"

if [ -e /home/russ/.nix-profile/etc/profile.d/nix.sh ]; then
    . /home/russ/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer


################################################################################
# Color fix
################################################################################

setopt PROMPT_SUBST


################################################################################
# Tmux
################################################################################

source ~/dotfiles/zsh/tmux.sh


################################################################################
# Autojump
################################################################################

# [[ autojump.sh ]] && \
#   source autojump


################################################################################
# Misc Aliases
################################################################################

alias vim=nvim
alias vi=nvim

alias ls=exa
alias l='ls -lFha'

alias e='emacs -nw'

alias ':q'='exit'



################################################################################
# Python
################################################################################

# eval "$(pyenv init -)"

################################################################################
# Docker
################################################################################

alias d=docker
alias dps='docker ps'
alias dpsa='docker ps -a'
alias drm='docker rm'
alias drmi='docker rmi'
alias drit='docker run -it'
alias drd='docker run -d'
alias di='docker images'
alias dc='docker-compose'
alias dcr='docker-compose restart'
alias dcl='docker-compose logs'
alias dclf='docker-compose logs -f'
alias dcrf='docker-compose restart "$1" && dclf $1'
alias dcps='dc ps'

################################################################################
# Kubernetes
################################################################################

alias kc='kubectl'
alias kg='kubectl get'
alias kp='kswitch'
alias kpods='kubectl get pods'
alias kdeploys='kubectl get deployments'
alias kdns='kubectl get ing'
alias kedit='kubectl edit deployments ' # the trailing space is intentional


################################################################################
# Git
################################################################################

alias gday='git log --name-only --since=yesterday.midnight --until=today.midnight --author="$(git config --get user.email)"'
alias glp="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gprom="git pull --rebase origin master"
alias gcan='git commit --amend --no-edit'
alias gcam='gc --amend'
alias gpf='git push --force'
alias gpff='git push --force --no-verify' # aka git push f***ing force
alias gds='git diff --staged'
alias gfx='git commit --fixup'
alias gri='git rebase -i --autosquash'
alias grim='git rebase -i --autosquash master'
alias gbm='git branch --merged'
alias gwip="git commit -m 'wip'"
alias pulls='open "https://github.com:/$(git remote -v | command ggrep -oP "'"(?<=git@github.com:).+(?=\.git)"'" | head -n 1)/pulls"'
alias git=hub
alias gpr='git pull-request'
alias gpo='git push -u origin'
alias gpob='git push -u origin "$(git symbolic-ref --short HEAD)"'
alias gcom='gco master'

# list recent branches
# thanks: https://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
function br() {
  git for-each-ref --count=5 --sort=-committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'
}

alias cwb='git symbolic-ref --short HEAD'

alias gwip="git commit -m 'wip'"

alias delete-merged-branches="git checkout master && git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d"


################################################################################
# Haskell
################################################################################

function stack-watch-test-path() {
  pattern="${1:-}"
  ghcid -c "stack ghci grid:lib grid:grid-test --ghci-options=-fobject-code" \
      --height=$(tput lines) --width=$(tput cols) \
      --warnings --test "${pattern}" \
      | source-highlight -s haskell -f esc
}

alias swtp='stack-watch-test-path'


export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# echo "end of .zshrc"
