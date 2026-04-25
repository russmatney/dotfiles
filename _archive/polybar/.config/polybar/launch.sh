#!/usr/bin/env bash

# Terminate already running bar instances
pkill polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# shellcheck source=/home/russ/.profile
source ~/.profile

# Launch Polybar, using default config location ~/.config/polybar/config
polybar -r bar1 &
polybar -r bar2 &

echo "Polybar launched..."
