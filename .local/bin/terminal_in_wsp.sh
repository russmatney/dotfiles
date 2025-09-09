#!/usr/bin/env sh


passed_wsp_name=$1

if [ "$passed_wsp_name" ]; then
    wsp_name=$passed_wsp_name
else
    wsp_name=$(hyprctl activeworkspace -j | jq -r ".name")
fi

# start_dir=$(clawebb wsp->dir "$wsp_name")
start_dir=$(clawebb -x clawe.cli/workspace-def-j --title "$wsp_name" | jq -r '.["workspace/directory"]')
start_dir=$(echo "$start_dir" | sed "s|^~|$HOME|")

alacritty -e tmux new -A -s "$wsp_name" -c "$start_dir"
