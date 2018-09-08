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


################################################################################
# Start up
################################################################################

# Restore pywal colors
~/.nix-profile/bin/wal -R

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
alias pulls='open "https://github.com:/$(git remote -v | /usr/local/bin/grep -oP "'"(?<=git@github.com:).+(?=\.git)"'" | head -n 1)/pulls"'
alias git=hub

################################################################################
# Kubernetes
################################################################################

alias kc='kubectl'
alias kpods='kubectl get pods'
alias kdeploys='kubectl get deployments'
alias kdns='kubectl get ing'
alias kedit='kubectl edit deployments ' # the trailing space is intentional
alias kswitch='gcloud container clusters get-credentials ' # the trailing space is intentional
