source ~/dotfiles/zsh/antigen-plugins.zsh

export PATH=$(brew --prefix coreutils)/libexec/gnubin:./node_modules/.bin:$PATH

alias vim='NVIM_TUI_ENABLE_TRUE_COLOR=true nvim'
alias vi='NVIM_TUI_ENABLE_TRUE_COLOR=true nvim'
alias ls='gls --color=always'

COMPLETION_WAITING_DOTS=true
DISABLE_AUTO_TITLE=true

# Git Aliases
alias 'glp'="git log --graph --pretty=format:'%Cred%h%Creset -%Cblue %an %Creset - %C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias 'gpb'="git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d"
alias 'gprom'="git pull --rebase origin master"
alias 'gcv'="git commit --verbose"

source ~/dotfiles/zsh/tmux-aliases.zsh
source ~/dotfiles/zsh/discovery.zsh

# Autojump `j`
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh

alias pg_start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias pg_stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

alias dm='docker-machine'
alias dc='docker-compose'

function dme() {
  eval "$(docker-machine env $1)"
}

function drm() {
  docker stop "$1";
  docker rm -v "$1";
}

alias exdollarbill='mix compile --force && mix credo --strict && mix dialyzer && mix test'

alias dps='docker ps -a'
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

#colorful less
if hash source-highlight 2>/dev/null; then
  export LESSOPEN="| src-hilite-lesspipe.sh %s"
  export LESS=" -R "
  alias less='less -m -g -i -J --underline-special --SILENT'
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

fpath=(~/.zsh $fpath)
