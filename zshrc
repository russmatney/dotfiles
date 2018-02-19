export META_DIR=~/urbint/meta

source "${META_DIR}/urbint_101/scripts/setup"

source /usr/local/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen theme https://gist.github.com/rschmukler/7d9317cde82c58be965d rs2.zsh-theme

antigen bundle zsh-users/zsh-syntax-highlighting

antigen bundle Tarrasch/zsh-autoenv
antigen bundle yerinle/zsh-gvm

antigen bundle brew
antigen bundle osx
antigen bundle nvm
antigen bundle rvm
antigen bundle docker
antigen bundle git
antigen bundle git-extras
antigen bundle sudo

antigen apply

export PATH=$(brew --prefix coreutils)/libexec/gnubin:./node_modules/.bin:$PATH

COMPLETION_WAITING_DOTS=true
DISABLE_AUTO_TITLE=true

# a bandaid if ever there was one
alias 'rmgil'="rm .git/index.lock"
alias 'grmil'="rm .git/index.lock"

alias 'pulls'='open "https://github.com:/$(git remote -v | /usr/local/bin/grep -oP "(?<=git@github.com:).+(?=\.git)" | head -n 1)/pulls"'

source ~/dotfiles/zsh/tmux-aliases.zsh

# Postgres
alias pg_start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pg_stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

#rust (installed via rustup)
export PATH="$HOME/.cargo/bin:$PATH"

#dat golang
if hash go 2>/dev/null; then
  export GOPATH=~/projects/go
  export PATH=`go env GOROOT`/bin:`go env GOPATH`/bin:$PATH
fi

# java 1.8 for clojure
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home


# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then source "$HOME/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then source "$HOME/google-cloud-sdk/completion.zsh.inc"; fi

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
