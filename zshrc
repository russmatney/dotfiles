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
antigen bundle bower
antigen bundle sudo

antigen apply

export PATH=$(brew --prefix coreutils)/libexec/gnubin:./node_modules/.bin:$PATH

alias vim='NVIM_TUI_ENABLE_TRUE_COLOR=true nvim'
alias vi='NVIM_TUI_ENABLE_TRUE_COLOR=true nvim'
# alias ls='gls --color=always'
alias ls='exa'

COMPLETION_WAITING_DOTS=true
DISABLE_AUTO_TITLE=true

# Git Aliases
alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias 'gpb'="git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d"
alias 'gprom'="git pull --rebase origin master"
alias 'gcv'="git commit --verbose"
alias 'gcan'="git commit --amend --no-edit"
alias 'gdsta'="git diff --staged"
alias 'gds'="git diff --staged"
alias 'gad'="git add ."


source ~/dotfiles/zsh/tmux-aliases.zsh
source ~/dotfiles/zsh/discovery.zsh

# Autojump `j`
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# Postgres
alias pg_start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pg_stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

# Elixir
alias exdollarbill='mix compile --force && mix credo --strict && mix dialyzer && mix test'
alias 'ism'="iex -S mix"
alias 'tism'="MIX_ENV=test iex -S mix"
alias 'mdg'='mix deps.get'
export ERL_AFLAGS="-kernel shell_history enabled"

# npm
alias 'nrd'='npm run dev'

# Docker
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
alias dls='docker images'
alias dmls='docker-machine ls'
alias dip='docker-machine ip'


function cm() {
  codemod -m -d "$1" --extensions "$2" --exclude-paths "/deps" "$3" "$4";
}

test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh

alias pip-upgrade-all="pip freeze --local | tee before_upgrade.txt | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

#rust (installed via rustup)
export PATH="$HOME/.cargo/bin:$PATH"

#dat golang
if hash go 2>/dev/null; then
  export GOPATH=~/projects/go
  export PATH=`go env GOROOT`/bin/:`go env GOPATH`/bin/:$PATH
fi

#elixir mix escripts
export PATH=$PATH:~/.mix/escripts

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export EDITOR=nvim

if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

    #colorful less
    if hash source-highlight 2>/dev/null; then
        export LESSOPEN="| src-hilite-lesspipe.sh %s"
        export LESS=" -R "
        alias less='less -m -g -i -J --underline-special --SILENT'
    fi
    export PAGER="less"

fpath=(~/.zsh $fpath)

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/russ/Downloads/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/russ/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/russ/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/russ/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
