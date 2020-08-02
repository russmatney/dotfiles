
# moved here to allow non-interactive modes to use it
export PATH="$HOME/.local/bin/:$HOME/n/bin:$PATH"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

source "/home/russ/.secrets"

eval $(luarocks path --bin)

export LUA_PATH="$LUA_PATH\
;/usr/share/awesome/lib/?.lua\
;/usr/share/awesome/lib/?/init.lua"
