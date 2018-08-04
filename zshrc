################################################################################
# Antibody setup

# source <(antibody init)

# antibody bundle < ~/dotfiles/.zsh_plugins.txt


################################################################################
# Gcloud

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/russ/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/russ/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/russ/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/russ/google-cloud-sdk/completion.zsh.inc'; fi


################################################################################
# Fzf

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


################################################################################
# Nix

if [ -e /home/russ/.nix-profile/etc/profile.d/nix.sh ]; then . /home/russ/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer


################################################################################
# Aliases

alias vim=nvim
alias vi=nvim

alias ls=exa


################################################################################
# Color fix

setopt PROMPT_SUBST


################################################################################
# Tmux

source ~/dotfiles/tmux.sh


################################################################################
# Autojump

[[ ~/.nix-profile/etc/profile.d/autojump.sh ]] && \
  source ~/.nix-profile/etc/profile.d/autojump.sh

