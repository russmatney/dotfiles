export BROWSER=/usr/bin/firefox
export TERMINAL=/usr/bin/alacritty
export EDITOR=/usr/bin/nvim

# lsp perf
# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
export LSP_USE_PLISTS=true

if test "$HOSTNAME" = 'algo' ; then
    export MONITOR=DP-4
elif test "$HOSTNAME" == 'anton' ; then
    export MONITOR=eDP-1
fi

# disable all husky git hooks
export HUSKY=0
