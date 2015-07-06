#TMUX aliases (ripped from @rschmukler)
tmux_search_paths=( ~/projects )

function tt() {
  sessionName=${1#*.}
  if ! tmux has-session -t "$sessionName" 2> ~/projects/null; then
    tmux_script=~/dotfiles/files/tmux-scripts/$1
    if [[ -e $tmux_script ]]; then
      zsh "$tmux_script"
    else
      oldTMUX=$TMUX
      unset TMUX
      tmux new -d -s $sessionName
      export TMUX=$oldTMUX
      unset oldTMUX
      for searches in $tmux_search_paths; do
        dir=$searches/$1
        if [[ -d $dir ]]; then
          tmux send-keys -t "${sessionName}" "cd $dir; clear" "C-m"
          break
        fi
      done
      unset searches
      unset tmux_scripts
      unset dir
    fi
  fi
  if [[ -n $TMUX ]]; then
    tmux switch-client -t $sessionName
  else
    tmux attach -t $sessionName
  fi
  unset sessionName
}

# gather files for auto-complete
function _tls() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
}
function _tscripts() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
  reply+=( $(ls ~/dotfiles/files/tmux-scripts) )
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
