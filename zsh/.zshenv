
# moved here to allow non-interactive modes to use it
export PATH="$HOME/.local/bin/:$HOME/n/bin:$PATH"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

export PATH="$HOME/.poetry/bin:$PATH"
(( $+commands[pyenv] )) && eval "$(pyenv init --path)"

source "/home/russ/.secrets"

[ -s ~/.luaver/luaver ] && . ~/.luaver/luaver
eval $(luarocks path --bin)

export LUA_PATH="$LUA_PATH\
;/usr/share/awesome/lib/?.lua\
;/usr/share/awesome/lib/?/init.lua"
if [ -e /home/russ/.nix-profile/etc/profile.d/nix.sh ]; then . /home/russ/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export SXHKD_SHELL=bg_shell
. "$HOME/.cargo/env"
