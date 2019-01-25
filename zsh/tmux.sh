################################################################################
# Exposing functions and aliases that make working with Tmux easier.
################################################################################



if ! command -v tmux >/dev/null; then
  echo 'Missing dependency (tmux). Please install and try again. Exiting...'
  return
fi



################################################################################
# Aliases
################################################################################

alias t='tt'
alias 't-'='tmux switch-client -l'
alias 'tt -'='tmux switch-client -l'
alias tls='tmux list-sessions'
alias tka='tmux kill-session -a'
alias tkt='tmux kill-session -a -t'



################################################################################
# Functions
################################################################################

tt() {
  # Find or create a Tmux session.
  local session_name="${1}"
  if ! tmux has-session -t "${session_name}" 2> /dev/null; then
    local oldTMUX="${TMUX}"
    unset TMUX
    tmux new -d -s "${session_name}" -n "${session_name}"
    export TMUX="${oldTMUX}"
    unset oldTMUX

    if command -v j >/dev/null; then
      tmux send-keys -t "${session_name}" "j ${session_name}; clear" "C-m"
    else
      tmux send-keys -t "${session_name}"
    fi
  fi
  if [[ -n "${TMUX}" ]]; then
    tmux switch-client -t "${session_name}"
  else
    tmux attach -t "${session_name}"
  fi
}


tk() {
  # `tk`: "tmux kill". Kills a tmux session by name.
  #
  # If no arguments are provided, kills the current session (after jumping to the previous session).
  #
  session_name="${1}"
  if [ ! -z "${session_name}" ]; then
    tmux kill-session -t "${session_name}"
  else
    session_name=tmux ls -f '#{?session_attached,#{session_name},}' | xargs
    tmux switch-client -l
    tmux kill-session -t "${session_name}"
  fi
}


################################################################################
# Autocomplete
################################################################################

function _tls_completions() {
    reply=( $(tmux list-sessions 2> ~/null | cut -d: -f1) )
}

compctl -K _tls_completions tk
compctl -K _tls_completions tt
