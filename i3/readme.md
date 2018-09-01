# i3 config

## Debugging tips

|-----------|--------------|--------------------------------|
| reload i3 | $mod-shift-r | Reload i3 (to test new config) |

## smart-focus-move

Smart movement between panes, splits, and applications.

> This bash script assumes you have `~/.local/bin` in your PATH

Ideally you could move around your tiling manager using relative
positioning alone - left, right, up, and down just do The Right Thing.

For me, this means supporting `Emacs`, `Vim`, and `Tmux` movement
before falling back to i3. Note that, to start, this is an
Arch-focused solution.
