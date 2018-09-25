#!/usr/bin/env bash

export EDITOR=nvim
# export EDITOR=emacsclient


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
autoload -U compinit && compinit

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

[[ ~/.nix-profile/etc/profile.d/autojump.sh ]] && \
  source ~/.nix-profile/etc/profile.d/autojump.sh


################################################################################
# Misc Aliases
################################################################################

alias vim=nvim
alias vi=nvim

alias ls=exa
alias l='ls -lFha'

alias e='emacs -nw'

alias ':q'='exit'


# restart emacs daemon
# TODO
# ps aux | grep emacs
# kill <emacs daemon pid>

################################################################################
# Python
################################################################################

eval "$(pyenv init -)"

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


################################################################################
# Git
################################################################################

alias gday='git log --name-only --since=yesterday.midnight --until=today.midnight --author="$(git config --get user.email)"'
alias glp="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gprom="git pull --rebase origin master"
alias gcan='git commit --amend --no-edit'
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

alias gwip="git commit -m 'wip'"


################################################################################
# Kubernetes
################################################################################

alias kc='kubectl'
alias kpods='kubectl get pods'
alias kdeploys='kubectl get deployments'
alias kdns='kubectl get ing'
alias kedit='kubectl edit deployments ' # the trailing space is intentional
alias kswitch='gcloud container clusters get-credentials ' # the trailing space is intentional


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

