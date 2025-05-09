#!/bin/sh

export XDG_CONFIG_HOME=$HOME/.config
export SHELL=/usr/bin/zsh

export BROWSER=/usr/bin/firefox

# make default editor Helix
# export EDITOR=helix
export EDITOR=/usr/bin/nvim

# Most pure GTK3 apps use wayland by default, but some,
# such as Firefox, require the backend to be explicitly selected.
# export MOZ_ENABLE_WAYLAND=1
# export MOZ_DBUS_REMOTE=1
# export GTK_CSD=0

# qt wayland
# export QT_QPA_PLATFORM="wayland"
# export QT_QPA_PLATFORMTHEME=qt5ct
# export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"

#Java XWayland blank screens fix
# export _JAVA_AWT_WM_NONREPARENTING=1

# set default shell and terminal
# export TERMINAL_COMMAND=xdg-terminal-exec
export TERMINAL=/usr/bin/alacritty

# add default location for zeit.db
# export ZEIT_DB="$HOME/.config/zeit.db"

# set ozone platform to wayland
# export ELECTRON_OZONE_PLATFORM_HINT=wayland

# Disable hardware cursors. This might fix issues with
# disappearing cursors
# if systemd-detect-virt -q; then
#     # if the system is running inside a virtual machine, disable hardware cursors
#     export WLR_NO_HARDWARE_CURSORS=1
# fi

# Disable warnings by OpenCV
# export OPENCV_LOG_LEVEL=ERROR

# set -a
# . "$HOME/.config/user-dirs.dirs"
# set +a

# if [ -n "$(ls "$HOME"/.config/profile.d 2>/dev/null)" ]; then
#     for f in "$HOME"/.config/profile.d/*; do
#         # shellcheck source=/dev/null
#         . "$f"
#     done
# fi


# lsp perf
# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
export LSP_USE_PLISTS=true

if test "$HOSTNAME" = 'algo' ; then
    export MONITOR=DP-4
fi

# disable all husky git hooks
export HUSKY=0
