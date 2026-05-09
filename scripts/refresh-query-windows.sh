#!/bin/bash

cache_file="/tmp/yabai_windows_cache.json"
temp_file="/tmp/yabai_windows_cache.tmp"
lock_file="/tmp/yabai_cache.lock"

refresh_cache() {
  # Use flock for locking to prevent simultaneous execution
  exec 200>"$lock_file" # Open file descriptor 200 for the lock file
  if ! flock -n 200; then
    echo "Another instance of the script is already running, skipping..."
    return
  fi

  echo "Refreshing cache..."

  # Record start time in seconds with sub-second precision
  start_time=$(perl -MTime::HiRes=time -e 'printf "%.3f\n", time')

  if yabai -m query --windows id,title,app > "$temp_file"; then
    mv "$temp_file" "$cache_file"

    # Record end time
    end_time=$(perl -MTime::HiRes=time -e 'printf "%.3f\n", time')

    # Calculate elapsed time
    elapsed_seconds=$(perl -e "printf \"%.3f\", $end_time - $start_time")

    echo "Cache refreshed successfully in ${elapsed_seconds}s at $(date)"
  else
    echo "Failed to refresh cache at $(date)"
    rm -f "$temp_file"
  fi

  # The lock is automatically released when the script exits
}

refresh_cache
