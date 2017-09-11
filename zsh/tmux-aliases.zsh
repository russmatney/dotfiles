#TMUX aliases (ripped from @rschmukler)

function tt() {
  sessionName=${1#*.}
  if ! tmux has-session -t "$sessionName" 2> ~/projects/null; then
    tmux_script=~/dotfiles/zsh/flows/$1
    if [[ -e $tmux_script ]]; then
      zsh "$tmux_script"
    else
      oldTMUX=$TMUX
      unset TMUX
      tmux new -d -s $sessionName -n $sessionName
      export TMUX=$oldTMUX
      unset oldTMUX

      tmux send-keys -t "${sessionName}" "j ${sessionName}; clear" "C-m"
    fi
    unset tmux_script
  fi
  if [[ -n $TMUX ]]; then
    tmux switch-client -t $sessionName
  else
    tmux attach -t $sessionName
  fi
  unset sessionName
}


tmux_search_paths=( ~/projects ~/projects/urbint )

# gather files for auto-complete
function _tls() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
}
function _tscripts() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
  reply+=( $(ls ~/dotfiles/zsh/flows) )
  for dir in $tmux_search_paths; do
    reply+=( $(ls $dir) )
  done
}

#handy functions
function tn() {
  tmux new -s "$1"
}
function tk() {
  tmux kill-session -t $1
}
function tm() {
  tmux new-session -t $1
}

# autocompletion attached to functions
compctl -K _tls ta
compctl -K _tls tk
compctl -K _tls tm
compctl -K _tscripts tt

alias tls="tmux list-sessions";
