#!/usr/bin/env bash


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
            < ~/dotfiles/zsh/.zsh_plugins.txt \
            > ~/dotfiles/zsh/.zsh_plugins.sh && \
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
if [ -f '/Users/russ/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/russ/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/russ/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/russ/google-cloud-sdk/completion.zsh.inc'; fi


################################################################################
# Fzf
################################################################################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


################################################################################
# Nix
################################################################################

if [ -e /home/russ/.nix-profile/etc/profile.d/nix.sh ]; then . /home/russ/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer


################################################################################
# Aliases
################################################################################

alias vim=nvim
alias vi=nvim

alias ls=exa
alias l='ls -lFha'

alias e='emacs -nw'

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

