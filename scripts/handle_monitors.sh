#!/usr/bin/env sh


update_monitors() {
    if grep -q open /proc/acpi/button/lid/*/state; then
        hyprctl keyword monitor eDP-1, highres, auto-up, 1
    else
        hyprctl keyword monitor eDP-1, disable
    fi
}

update_monitors

# while true; do
#     inotifywait -q -e modify /proc/acpi/button/lid/*/state 2>/dev/null

#     update_monitors
#     echo "Updating monitor state"
# done
