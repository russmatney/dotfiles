#TMUX aliases (ripped from @rschmukler)

# gather files for auto-complete
function _tls() {
  reply=( $(tmux list-sessions 2> ~/projects/null | cut -d: -f1) )
}

# autocompletion attached to functions
compctl -K _tls ta
compctl -K _tls tk
compctl -K _tls tm
compctl -K _tscripts tt

alias jj="tt";
