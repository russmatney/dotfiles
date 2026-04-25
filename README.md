# dotfiles

Personal config managed with a custom Babashka symlink manager (`dots.bb`) and
NixOS for system-level packages and services.

## `bb dots` — the symlink manager

```bash
bb dots link           # apply all links from dots.edn
bb dots status         # show link state (linked / missing / broken)
bb dots unlink         # remove managed symlinks
bb dots link --dry-run # preview without touching the filesystem
```

Links are declared in `dots.edn`. The tool reads `/etc/hostname` to determine
the current machine and merges any per-machine links on top of the shared set.
Override with `$DOTS_HOSTNAME`.

### `dots.edn` schema

```edn
:links   [{:from "repo/relative/path"  :to "~/.destination"}
           {:from "some/dir"           :to "~/.config/foo" :link-contents true}]

:machines {"hostname" [{:from "..." :to "..."}]}
```

- `:from` — path relative to the repo root
- `:to` — symlink destination; `~/` expands to `$HOME`
- `:link-contents true` — links each file inside `:from` individually (like `stow`)
- `:machines` — per-hostname overrides, merged on top of `:links` at runtime

---

## What's wired up

### Shared links

| Source | Destination | Notes |
|---|---|---|
| `config/foot` | `~/.config/foot` | Wayland terminal |
| `config/alacritty` | `~/.config/alacritty` | Cross-platform terminal |
| `config/nvim` | `~/.config/nvim` | AstroVim (Lua) |
| `config/nvim.fnl` | `~/.config/nvim.fnl` | Fennel nvim config |
| `config/emacs/doom` | `~/.config/doom` | Doom Emacs |
| `config/emacs/applications` | `~/.local/share/applications` | `.desktop` files, link-contents |
| `config/hypr` | `~/.config/hypr` | Hyprland WM |
| `config/rofi` | `~/.config/rofi` | App launcher |
| `config/tmux/powerline` | `~/.config/powerline` | Tmux Powerline theme |
| `config/tmux/systemd/user` | `~/.config/systemd/user` | Tmux systemd unit, link-contents |
| `home/.tmux.conf` | `~/.tmux.conf` | |
| `home/.zshenv` | `~/.zshenv` | |
| `home/.profile` | `~/.profile` | |
| `home/.gitconfig` | `~/.gitconfig` | |
| `config/zsh` | `~/.config/zsh` | Zsh config + themes, link-contents |
| `config/zsh/completion` | `~/.config/zsh/completion` | Completions dir |
| `scripts` | `~/.local/bin` | All scripts, link-contents |

### Per-machine links (Hyprland monitor layout)

Each machine gets its own monitor config and overrides linked into `~/.config/hypr/`:

| Machine | Source |
|---|---|
| `yoshi` | `config/hypr/machines/yoshi/` |
| `brain` | `config/hypr/machines/brain/` |
| `walt` | `config/hypr/machines/walt/` |
| `wordnerd` | `config/hypr/machines/wordnerd/` |

---

## NixOS

`nixos/` is used directly — no symlinks. The flake at `nixos/flake.nix` is the
entry point. Machine configs live in `nixos/machines/<hostname>.nix`.

```bash
sudo nixos-rebuild switch --flake /home/russ/dotfiles/nixos#yoshi
```

Packages and services are managed through NixOS modules (`nixos/modules/`).
Adding a new tool means editing a module there, not installing manually.

---

## macOS

Hammerspoon (`hammerspoon/`) and yabai/skhd (`yabai/`) configs exist in the
repo but are not wired into `dots.edn` yet. Migration is deferred until back on
`walt`. The plan leans toward hammerspoon as the primary macOS WM layer.

See `sessions/2025-04-25-dots-migration.md` for the full porting roadmap.
