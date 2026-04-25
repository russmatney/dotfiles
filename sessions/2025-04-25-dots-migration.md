# Dots Migration Plan
_2025-04-25_

Goal: consolidate all remaining stow-style directories into `dots.edn`, clean
up the repo root, and leave only things that genuinely don't belong there.

The test for "done": `bb dots status` shows everything green and the only
top-level dirs are `config/`, `home/`, `scripts/`, `nixos/`, `_archive/`,
`.claude/`, and the handful of explicitly-deferred macOS dirs.

---

## Trivial ports

Single files or single `~/.config/<tool>` dirs. Each is a move + one `dots.edn`
entry. Low risk, do these in a batch.

### `atuin/`

Move `.config/atuin/` into `config/`:

```bash
mv atuin/.config/atuin config/atuin
rmdir -p atuin/.config
```

`dots.edn` entry:
```edn
{:from "config/atuin" :to "~/.config/atuin"}
```

---

### `fennel/`

The `.fennelrc` is a dotfile in `$HOME`, leave it in place and just add an
entry:

```edn
{:from "fennel/.fennelrc" :to "~/.fennelrc"}
```

No file moves needed. The `fennel/` dir stays, just gets wired up.

---

### `haskell/`

Same pattern as fennel — `.ghci` lives in `$HOME`:

```edn
{:from "haskell/.ghci" :to "~/.ghci"}
```

---

### `psql/`

```edn
{:from "psql/.psqlrc" :to "~/.psqlrc"}
```

---

### `readline/`

```edn
{:from "readline/.inputrc" :to "~/.inputrc"}
```

---

### `rip-grep/`

`.ignore` is the global ripgrep ignore file, lives at `~/.ignore`:

```edn
{:from "rip-grep/.ignore" :to "~/.ignore"}
```

---

### `lua/` (luaformatter)

Move into `config/`:

```bash
mkdir -p config/luaformatter
mv lua/.config/luaformatter/config.yaml config/luaformatter/config.yaml
rmdir -p lua/.config/luaformatter
```

```edn
{:from "config/luaformatter" :to "~/.config/luaformatter"}
```

---

### `neofetch/`

```bash
mv neofetch/.config/neofetch config/neofetch
rmdir -p neofetch/.config
```

```edn
{:from "config/neofetch" :to "~/.config/neofetch"}
```

---

### `notifications/` (deadd)

```bash
mv notifications/.config/deadd config/deadd
rmdir -p notifications/.config
```

```edn
{:from "config/deadd" :to "~/.config/deadd"}
```

---

## Small moves

Require a dir relocation but still a single `dots.edn` entry.

### `waybar/`

Actively maintained. Move alongside hypr/rofi/foot in `config/`:

```bash
mv waybar config/waybar
```

```edn
{:from "config/waybar" :to "~/.config/waybar"}
```

---

### `python/`

Two separate destinations — `.config/flake8` and `.ipython/`:

```bash
mkdir -p config/python
mv python/.config/flake8 config/python/flake8
mv python/.ipython config/python/ipython
rmdir -p python/.config
```

```edn
{:from "config/python/flake8"  :to "~/.config/flake8"}
{:from "config/python/ipython" :to "~/.ipython"}
```

---

## Medium moves

### `clojure/`

Three things with different destinations:

```bash
mkdir -p config/clojure
mv clojure/.clojure/deps.edn          config/clojure/deps.edn
mv clojure/.clojure/rebel_readline.edn config/clojure/rebel_readline.edn
# systemd unit goes alongside the tmux unit pattern:
mv clojure/.config/systemd/user/bb-nrepl.service config/clojure/systemd/user/bb-nrepl.service
rmdir -p clojure/.clojure clojure/.config/systemd/user
```

```edn
{:from "config/clojure/deps.edn"           :to "~/.clojure/deps.edn"}
{:from "config/clojure/rebel_readline.edn" :to "~/.clojure/rebel_readline.edn"}
{:from "config/clojure/systemd/user"       :to "~/.config/systemd/user" :link-contents true}
```

Note: the last entry merges with the existing tmux systemd link-contents entry —
verify no conflicts on apply.

---

### `ranger/`

Config files move to `config/ranger/`; the two bin scripts fold into `scripts/`
(already linked to `~/.local/bin`):

```bash
mv ranger/.config/ranger config/ranger
mv ranger/.local/bin/ranger-cd   scripts/ranger-cd
mv ranger/.local/bin/ranger_here scripts/ranger_here
rmdir -p ranger/.config ranger/.local/bin
```

```edn
{:from "config/ranger" :to "~/.config/ranger"}
```

The scripts pick up automatically via the existing `scripts/ → ~/.local/bin`
link-contents entry.

---

### `nvidia/`

No config dir — just two shell scripts. Move into `scripts/` directly:

```bash
mv nvidia/.local/bin/disable-gpu.sh scripts/disable-gpu
mv nvidia/.local/bin/enable-gpu.sh  scripts/enable-gpu
rmdir -p nvidia/.local/bin
```

No new `dots.edn` entry needed — scripts are already covered. Delete the
empty `nvidia/` dir afterward.

---

## Verify before porting

### `clawe/` systemd units

`clawe/.config/systemd/user/` has four service files:
- `doctor-be.service`
- `doctor-dashboard.service`
- `doctor-fe.service`
- `doctor-topbar.service`

**Check first:** `nixos/modules/clawe.nix` may already be generating and
installing these via NixOS. If so, the dotfiles copies are dead and can be
deleted. If not managed by Nix, move to `config/clawe/systemd/user/` and add
a link-contents entry.

```bash
# Check if NixOS is managing them:
grep -r "doctor" nixos/modules/clawe.nix
systemctl --user status doctor-be   # is it running? where is the unit loaded from?
```

---

### `hypr/` (repo root)

There's a split: `config/hypr/machines/` has yoshi and brain overrides; the
root-level `hypr/machines/` has walt and wordnerd. The `dots.edn` `:machines`
entries for walt and wordnerd point into `config/hypr/machines/walt/` and
`config/hypr/machines/wordnerd/` — but those files might not exist yet (they
exist under the root `hypr/`).

**Check first:**
```bash
ls config/hypr/machines/
ls hypr/machines/
```

If the files aren't in `config/hypr/machines/`, merge them:
```bash
cp -r hypr/machines/walt     config/hypr/machines/walt
cp -r hypr/machines/wordnerd config/hypr/machines/wordnerd
rm -r hypr/
```

---

## Archive candidates

These dirs are dead or superseded. Move to `_archive/` rather than deleting
outright, in case anything useful needs rescuing later.

| Dir | Reason |
|---|---|
| `X/` | X11 startup files (`.xinitrc`, `.Xmodmap`, `.xprofile`) — superseded by Wayland/Hyprland |
| `sxhkd/` | sxhkd systemd unit — superseded by Hyprland `bindings.conf` |
| `feh/` | Empty dir (`.config/buttons` only) — no meaningful content |
| `clawe/Library/LaunchAgents/` | macOS launchd plists — superseded by NixOS systemd on Linux; defer full delete until macOS is re-evaluated |

```bash
mkdir -p _archive
mv X sxhkd feh _archive/
# clawe macOS plist dir:
mv clawe/Library _archive/clawe-launchagents
```

---

## macOS (deferred — targeting `walt`)

The following dirs exist and are intentionally left alone for now:

| Dir | Contents |
|---|---|
| `hammerspoon/` | `.hammerspoon/init.lua` — Lua WM config, actively used on macOS |
| `yabai/` | `.yabairc`, `.skhdrc`, `refresh-query-windows.sh` — tiling WM + hotkeys |
| `osx/` | `osx-hacks.sh`, `material-dark.itermcolors` |

When ready to port for `walt`:

1. Add `walt` (or the macOS hostname) to `dots.edn` `:machines` with appropriate entries
2. `dots.bb` needs a small tweak to handle macOS home dir and `~/.hammerspoon` vs `~/.config` conventions
3. Decide hammerspoon vs yabai — currently leaning hammerspoon-first; yabai can stay
   wired up but hammerspoon takes the WM role
4. `config/alacritty/alacritty.osx.toml` already exists — wire it up as a machine-specific link

---

## Doing the work

Suggested order:

1. **Trivial batch** — fennel, haskell, psql, readline, rip-grep (entries only, no moves)
2. **Move + entry batch** — atuin, lua, neofetch, notifications, waybar
3. **Verify** — clawe systemd, hypr root dir split
4. **Medium** — clojure, python, ranger, nvidia scripts
5. **Archive** — X, sxhkd, feh, clawe Library
6. **macOS** — when on walt

Run `bb dots status` after each batch to confirm nothing is broken.
