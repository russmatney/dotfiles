export BROWSER=/usr/bin/firefox
export TERMINAL=/usr/bin/alacritty
export EDITOR=/usr/bin/nvim

if test "$HOSTNAME" = 'algo' ; then
    export MONITOR=HDMI-0
elif test "$HOSTNAME" == 'vader' ; then
    export MONITOR=eDP-1
fi
