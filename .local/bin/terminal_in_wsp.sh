#!/usr/bin/env sh


passed_wsp_name=$1

if [ "$passed_wsp_name" ]; then
    wsp_name=$passed_wsp_name
else
    wsp_name=$(hyprctl activeworkspace -j | jq -r ".name")
fi

# TODO impl clawebb helper like:
# start_dir=$(clawebb wsp->dir "$wsp_name")

alacritty -e tmux new -A -s "$wsp_name"
# -c $start_dir
