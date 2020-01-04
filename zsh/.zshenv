
# moved here to allow non-interactive modes to use it
export PATH="$HOME/.local/bin/:$HOME/n/bin:$PATH"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

if [[ $HOST == 'algo' ]]; then
    export MONITOR=HDMI-0
fi
if [[ $HOST == 'vader' ]]; then
    export MONITOR=eDP-1
fi

source "/home/russ/.secrets"
