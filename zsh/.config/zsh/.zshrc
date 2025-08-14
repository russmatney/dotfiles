#!/usr/bin/env bash
# TODO add notes up here with links to zsh-managment docs

# enable profiling
# zmodload zsh/zprof

################################################################################
# Environment Vars
################################################################################

# export EDITOR='emacsclient -nw'
# export VISUAL='emacsclient -nw'
export EDITOR='nvim'
export VISUAL='nvim'
export PAGER='less'

export RANGER_LOAD_DEFAULT_RC=FALSE

export FONTCONFIG_PATH='/etc/fonts'

alias 'zz'='source ~/.config/zsh/.zshrc'

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE

# Antidote ################################################################################

if [ -f /usr/share/zsh-antidote/antidote.zsh ]; then
  source '/usr/share/zsh-antidote/antidote.zsh'
  antidote load
elif [ -f /opt/homebrew/opt/antidote/share/antidote/antidote.zsh ]; then
  source /opt/homebrew/opt/antidote/share/antidote/antidote.zsh
  antidote load
elif [ -f /run/current-system/sw/share/antidote/antidote.zsh ]; then
  source '/run/current-system/sw/share/antidote/antidote.zsh'
  antidote load
fi

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

# enable completion features
fpath=(~/.config/zsh/completion $fpath)
autoload -Uz compinit && compinit -i

# allow .hidden file tab completion
setopt globdots

# required to find some shared libs
export LD_LIBRARY_PATH=/usr/local/lib

################################################################################
# Keychain
################################################################################

# keychain for ssh and gpg
if hash keychain 2>/dev/null; then
  keychain_path="~/.keychain/$(hostname)-sh-gpg";  # this file won't exist if keychain isn't running
  if [ -f "$keychain_path" ]; then
    . $keychain_path;
  else

    case "$OSTYPE" in
      darwin*)
        eval $(keychain --eval --noask)
        ;;
      linux*)
        eval $(keychain --eval --systemd --noask)
        ;;
    esac
  fi;
fi

################################################################################
# Theme/Style
################################################################################

# Restore pywal colors
# ~/.nix-profile/bin/wal -Rq
alias wa='wal -Req'

alias ns='nix-shell'
alias nrs='sudo nixos-rebuild switch'

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

my-paste() {
  xclip -o
}
autoload my-paste
zle -N my-paste

choose-session() {
  tmux choose-session
}
autoload choose-session
zle -N choose-session

bindkey -M vicmd 'k' to-tmux-copy-mode \
                 'q' to-insert-mode \
                 '\e' to-insert-mode \
                 '^r' ctrl-r \
                 '/' search-page \
                 '?' search-page-up \
                 'p' my-paste \
                 '-' choose-session

################################################################################
# Gcloud
################################################################################

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/russ/google-cloud-sdk/path.zsh.inc' ]; then
    source '/home/russ/google-cloud-sdk/path.zsh.inc';
fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/russ/google-cloud-sdk/completion.zsh.inc' ]; then
    source '/home/russ/google-cloud-sdk/completion.zsh.inc';
fi

if [ -f  "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc" ]; then
    source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc";
fi

if [ -f  "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc" ]; then
    source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc";
fi

################################################################################
# Fzf
################################################################################

[ -f /usr/bin/fzf ] &&
    source /usr/share/fzf/completion.zsh
[ -f /usr/bin/fzf ] &&
    source /usr/share/fzf/key-bindings.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[  -f /usr/bin/fzf ] && source ~/.config/zsh/fzf.zsh

source ~/.config/zsh/fuzzy-sys.zsh

################################################################################
# Color fix
################################################################################

setopt PROMPT_SUBST
export RPROMPT='%@'


################################################################################
# Tmux
################################################################################

alias t=$(which tt)
alias tk='tt tk'
alias 't-'='tmux switch-client -l'
alias 't -'='tmux switch-client -l'
alias 'tt -'='tmux switch-client -l'
alias tls='tmux list-sessions'
alias tka='tmux kill-session -a'
alias tkt='tmux kill-session -a -t'

################################################################################
# Misc Aliases
################################################################################

alias vim=nvim
alias vi=nvim

alias ls=eza
alias l='ls -lha'

alias cat=bat

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
alias dc='docker-compose'
alias dcr='docker-compose restart'
alias dcud='docker-compose up -d'
alias dcub='docker-compose up -d --build'
alias dcl='docker-compose logs'
# unalias dclf
function dclf() {
  docker-compose logs -f "$@" | cut -f2- -d'|'
}
function dcrf() {
  docker-compose restart "$@"
  docker-compose logs -f "$@" | cut -f2- -d'|'
}
alias dcps='dc ps'

################################################################################
# Kubernetes
################################################################################

alias kc='kubectl'
alias kca='kubectl --all-namespaces'
alias kcd='kubectl --namespace=default'
alias kg='kubectl get'
alias kpods='kubectl get pods'
alias kpodsa='kubectl get pods --all-namespaces'
alias kings='kubectl get ing'
alias kingsa='kubectl get pods --all-namespaces'
alias kdeploys='kubectl get deployments'
alias kdns='kubectl get ing'
alias kedit='kubectl edit deployments ' # the trailing space is intentional
alias kt='kubetail'

################################################################################
# Git
################################################################################

alias gday='git log --name-only --since=yesterday.midnight --until=today.midnight --author="$(git config --get user.email)"'
alias glp="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gprom="git pull --rebase origin master"
alias gcan='git commit --amend --no-edit'
alias gcam='gc --amend'
alias gpf='git push --force-with-lease'
alias gpff='git push --force-with-lease --no-verify' # aka git push f***ing force
alias gds='git diff --staged'
alias gfx='git commit --fixup'
alias gri='git rebase -i --autosquash'
alias grim='git rebase -i --autosquash master'
alias gbm='git branch --merged'
alias gwip="git commit -m 'wip'"
alias pulls='open "https://github.com:/$(git remote -v | command ggrep -oP "'"(?<=git@github.com:).+(?=\.git)"'" | head -n 1)/pulls"'
# alias git=hub
alias gpr='git pull-request'
alias gpo='git push -u origin'
alias gpob='git push -u origin "$(git symbolic-ref --short HEAD)"'
alias gcom='gco master'
alias gclone='git clone'

# list recent branches
# thanks: https://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
function br() {
  git for-each-ref --count=5 --sort=-committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'
}

alias cwb='git symbolic-ref --short HEAD'

alias gwip="git commit -m 'wip'"

alias delete-merged-branches="git checkout master && git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d"

# from jskrzypek
# alias is-unmerged="! f() { local branch=$(git rev-parse --abbrev-ref ${1-HEAD}) ; local base=${2-master} ; echo $(git cherry $base $(git commit-tree $(git rev-parse $branch^{tree}) -p $(git merge-base $base $branch) -m _) | cut -f1 -d' ') $branch; }; f"
alias branch-merged="! f() { git for-each-ref refs/heads/ '--format=%(refname:short)' | xargs -I {} git is-unmerged {} ${1-master} | grep -E '^-' | cut -f2 -d' ' ; }; f"
alias clean-merged-branches="!f() { git checkout -q ${1-master} && git branch-merged ${1-master} | xargs -n1 -p git branch -D; }; f"


################################################################################
# Systemctl
################################################################################

case "$OSTYPE" in
  linux*)
    alias sc='systemctl'
    alias scu='systemctl --user'
    alias scr='systemctl restart'
    alias scur='systemctl --user restart'
    alias scs='systemctl status'
    alias jc='journalctl'
    alias jcf='journalctl -f'
    alias jcu='journalctl --user-unit'
    alias jcfu='journalctl -f --lines 500 --user-unit'
esac

################################################################################
# Misc
################################################################################

alias jk='bat ./readme.org'
alias nf='neofetch'
alias pd='pandoc'

source ~/.config/zsh/grfn.zsh-theme

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

################################################################################
# Node
################################################################################

export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use  # faster zsh startup
# function loadnvm() {
#   [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # call when you want it
# }

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

################################################################################
# Rust
################################################################################

export PATH="$HOME/.cargo/bin:$PATH"

alias ps=procs
alias du=dua

# zoxide #########################################################################

export _ZO_ECHO=1
[ -f ~/.cargo/bin/zoxide ] && eval "$(zoxide init zsh --cmd cd)"

################################################################################
# Doom emacs
################################################################################

export PATH="$HOME/.config/emacs/bin:$PATH"

# Added by n-install (see http://git.io/n-install-repo).
export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"


################################################################################
# misc
################################################################################

# unalias 'sp'

################################################################################
# Lua
################################################################################

function useluaver() {
  [ -s ~/.luaver/luaver ] && . ~/.luaver/luaver
}


################################################################################
# Delete from history via fzf
################################################################################

# https://superuser.com/questions/1316668/zsh-bash-delete-specific-lines-from-history
function delete-command () {
  # Prevent the specified history line from being saved.
  local HISTORY_IGNORE="${(b)$(fc -ln $1 $1)}"

  # Write out the history to file, excluding lines that match `$HISTORY_IGNORE`.
  fc -W

  # Dispose of the current history and read the new history from file.
  fc -p "$HISTFILE" "$HISTSIZE" "$SAVEHIST"

  # TA-DA!
  print "Deleted '$HISTORY_IGNORE' from history."
}

function pick_from_history () {
  history | fzf --tac --tiebreak=index | perl -ne 'm/^\s*([0-9]+)/ and print "$1"'
}

function delete_from_history () {
  delete-command "$(pick_from_history)"
}

################################################################################
# Ruby
################################################################################

export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"

eval "$(direnv hook zsh)"

################################################################################
# git summary
################################################################################

#[[file:~/MirkoLedda/git-summary/README.md::git-summary][local git-summary readme]]

alias git-summary='~/MirkoLedda/git-summary/git-summary'

################################################################################
# babashka
################################################################################

_bb_tasks() {
    local matches=(`bb tasks | tail -n +3 | cut -f1 -d ' '`)
    compadd -a matches
    _files # autocomplete filenames as well
}
compdef _bb_tasks bb

export PATH="$PATH:$HOME/.babashka/bbin/bin"

################################################################################
# gg
################################################################################

if [ -f "$HOME/russmatney/gg/bb.edn" ]; then
  unalias gg
  export PATH="$PATH:$HOME/russmatney/gg/bin"

  _gg_tasks() {
      local matches=(`bb tasks | tail -n +3 | cut -f1 -d ' '`)
      compadd -a matches

      local gg_matches=(`bb --config ~/russmatney/gg/bb.edn tasks | tail -n +3 | cut -f1 -d ' '`)
      compadd -a gg_matches
      _files # autocomplete filenames as well
  }
  compdef _gg_tasks gg
fi

#########################################################################
# clawe
#########################################################################

# now just clawebb in ~/.local/bin/clawebb
# alias clawebb='bb --config ~/russmatney/clawe/bb.edn'

# brainstorm ways to complete `clawebb -x ns/fn --blah arg`
if [ -f "$HOME/russmatney/clawe/bb.edn" ]; then
  _clawe_tasks() {
      local clawe_matches=(`bb --config ~/russmatney/clawe/bb.edn tasks | tail -n +3 | cut -f1 -d ' '`)
      compadd -a clawe_matches
  }
  compdef _clawe_tasks clawebb
  compdef _clawe_tasks clawebb-log
fi


################################################################################
# Python
################################################################################

# already on path to due to .zshenv
if (( $+commands[pyenv] )); then
   eval "$(pyenv init -)";
   eval "$(pyenv virtualenv-init -)";
   test ! -f ~/.pyenv/version && pyenv global system;
fi

################################################################################
# perf
################################################################################

timezsh() {
  local shell=${1-$SHELL}
  for i in $(seq 1 10); do time $shell -i -c exit; done
}

# print profiling
# zprof

################################################################################
# aseprite
################################################################################

PATH=$PATH:~/.local/share/Steam/steamapps/common/Aseprite

case "$OSTYPE" in
  darwin*)
    alias aseprite=~/Library/Application\\\ Support/Steam/steamapps/common/Aseprite/Aseprite.app/Contents/MacOS/aseprite
esac

################################################################################
# godot
################################################################################

# gut without window
alias gut='godot --headless --path . -d -s addons/gut/gut_cmdln.gd'
# gut with window
alias gutw='godot --path . -d -s addons/gut/gut_cmdln.gd'

case "$OSTYPE" in
  darwin*)
    alias godotsteam='~/.local/share/godotsteam/GodotSteam.app/Contents/MacOS/godot'
    ;;
  linux*)
    alias godotsteam='~/.local/share/godotsteam/godotsteam.441.editor.linux.x86_64'
    ;;
esac


#########################################################################
# homebrew

case "$OSTYPE" in
  darwin*)
    export HOMEBREW_NO_AUTO_UPDATE=1

    # gls in emacs
    PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"

    # java for clojure/babashka
    PATH="/opt/homebrew/opt/openjdk/bin:$PATH"

    # homebrew
    alias b="brew"
    alias bs="brew services"
    alias bsr="brew services restart"
    alias bsl="brew services list"
    alias bi="brew install"
    alias bu="brew uninstall"
    alias binf="brew info"

    # autojump
    [ -f /opt/homebrew/etc/profile.d/autojump.sh ] && . /opt/homebrew/etc/profile.d/autojump.sh
esac

#########################################################################
# itch.io


# https://itch.io/docs/butler/pushing.html#appendix-b-beeps-4-life
# alias butler='~/.config/itch/apps/butler/butler --beeps4life'
case "$OSTYPE" in
  darwin*)
    export PATH="$PATH:$HOME/Library/Application Support/itch/apps/butler"
    ;;
  linux*)
    export PATH="$PATH:$HOME/.config/itch/apps/butler"
    ;;
esac

# atuin #########################################################################

eval "$(atuin init zsh)"

# dotnet #########################################################################

export DOTNET_CLI_TELEMETRY_OPTOUT=1
export PATH="$PATH:$HOME/.dotnet/tools"

case "$OSTYPE" in
  darwin*)
    export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"
    ;;
  linux*)
    ;;
esac
