# CTRL-Y - Paste the selected branch(es) into the command line
__bsel() {
  local cmd="git branch -a | tr -d '* ' | sed 's/^remotes\/origin\///' | sort | uniq"
  setopt localoptions pipefail 2> /dev/null
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read item; do
    echo -n "${(q)item} "
  done
  local ret=$?
  echo
  return $ret
}

fzf-branch-widget() {
  LBUFFER="${LBUFFER}$(__bsel)"
  local ret=$?
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}
zle     -N   fzf-branch-widget
bindkey '^Y' fzf-branch-widget
alias 'gbz'='fzf-branch-widget'
