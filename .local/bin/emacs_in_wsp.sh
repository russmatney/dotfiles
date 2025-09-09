#!/usr/bin/env sh

passed_wsp_name=$1

if [ "$passed_wsp_name" ]; then
    wsp_name=\"$passed_wsp_name\"
else
    # note this wsp includes quotes b/c it's not -r 'raw', it's json formatted
    wsp_name=$(hyprctl activeworkspace -j | jq ".name")
fi

echo $wsp_name

# TODO impl clawebb helper like:
# initial_file=$(clawebb wsp->first-file "$wsp_name")

# start_dir=$(clawebb -x clawe.cli/workspace-def-j --title "$wsp_name" | jq -r '.["workspace/directory"]')
# start_dir=$(echo "$start_dir" | sed "s|^~|$HOME|")

emacsclient --no-wait --create-frame \
    -F "((name . $wsp_name))" \
    --eval "(progn (russ/open-workspace $wsp_name))"
# (find-file \"$initial_file\")
