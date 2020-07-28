export BROWSER=/usr/bin/google-chrome-stable
export TERMINAL=/usr/bin/alacritty
export EDITOR=/usr/bin/nvim

eval $(luarocks path --bin)

if test "$HOSTNAME" = 'algo' ; then
    export MONITOR=HDMI-0
elif test "$HOSTNAME" == 'vader' ; then
    export MONITOR=eDP-1
fi
