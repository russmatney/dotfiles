################################################################################
# PATH
################################################################################

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
export PATH=./node_modules/.bin:$PATH


################################################################################
# Misc
################################################################################

# a bandaid if ever there was one
alias 'rmgil'="rm .git/index.lock"
alias 'grmil'="rm .git/index.lock"


################################################################################
# Gcloud
################################################################################

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then source "$HOME/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then source "$HOME/google-cloud-sdk/completion.zsh.inc"; fi

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
