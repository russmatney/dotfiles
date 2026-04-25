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

- **nvim** — AstroNvim (Lua); fennel config also linked separately
- **doom** — Doom Emacs
- **hypr** — Hyprland WM; per-machine monitor/override confs merged at link time
- **waybar** — Wayland status bar
- **foot** — Wayland terminal
- **alacritty** — cross-platform terminal
- **rofi** — app launcher
- **zsh** — config, themes, completions
- **tmux** — `.tmux.conf` + Powerline theme
- **atuin** — shell history
- **clojure** — `deps.edn`, `rebel_readline.edn`
- **flake8**, **luaformatter** — linter/formatter configs
- **nixpkgs** — user-level nixpkgs config
- **ipython**, **ghci**, **psql** — REPL configs
- **scripts** — all `scripts/` wired to `~/.local/bin`
- **dotfiles** — `~/.fennelrc`, `~/.gitconfig`, `~/.zshenv`, `~/.profile`, `~/.inputrc`, `~/.ignore`

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
