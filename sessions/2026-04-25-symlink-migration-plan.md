# Dotfiles Symlink Migration Plan
**Date:** 2026-04-25
**Status:** Ready for execution
**Context:** Derived from `2026-04-25-redesign-braindump.md` — part of the broader dotfiles
simplification effort (new baby, new job, less time for deep tinkering).

---

## Goal

Replace GNU Stow with a Babashka-driven symlink manager (`dots.bb` + `dots.edn`).
Flatten the stow-buried directory structure into a readable `config/` + `home/` layout.
Archive dead window manager configs. Migrate one tool at a time to avoid breaking the
working environment.

---

## Background / Constraints

- **No home-manager expansion.** home-manager is present minimally and stays that way.
  Plugin/symlink management lives in `.edn`, `.bb`, or `.clj` files run via `bb`.
- **tmux stays as-is.** TPM bootstrap kept — tmux runs on non-nix machines. No nixification.
- **yabai + hammerspoon stay.** Still used on macOS. Do not archive.
- **One tool at a time.** Each phase must be committed and verified before the next begins.
- **Rollback path always open.** `bb dots unlink` + manual `stow <pkg>` must always be
  able to restore the previous state until the stow package dir is deleted.
- **No commits on agent's behalf.** User commits manually after each verified phase.

---

## New Directory Layout

```
dotfiles/
├── config/          ← replaces stow packages; each subdir → ~/.config/<name>
│   ├── zsh/
│   ├── nvim/
│   ├── hypr/
│   ├── foot/
│   ├── alacritty/
│   ├── tmux/
│   └── emacs/       ← was emacs/.config/doom/
├── home/            ← files that land at $HOME root
│   ├── .tmux.conf
│   ├── .zshenv
│   └── .gitconfig
├── dots.edn         ← symlink manifest (source of truth)
├── dots.bb          ← symlink manager script
├── bb.edn           ← bb task runner (wires in dots)
├── _archive/        ← dead WM configs, kept for reference
│   ├── awesome/
│   ├── i3/
│   ├── sway/
│   ├── chunkwm/
│   ├── compton/
│   ├── picom/
│   ├── rofi/
│   ├── polybar/
│   └── i3-scrot/
├── yabai/           ← keep (macOS)
├── hammerspoon/     ← keep (macOS)
├── nixos/           ← unchanged
└── ...              ← other stow packages migrate incrementally
```

---

## dots.edn — Manifest Shape

```clojure
{:dotfiles-dir "/home/russ/dotfiles"   ; or detect via (System/getenv "DOTFILES") / git root

 :links
 [{:from "config/zsh"        :to "~/.config/zsh"}
  {:from "config/nvim"       :to "~/.config/nvim"}
  {:from "config/hypr"       :to "~/.config/hypr"}
  {:from "config/foot"       :to "~/.config/foot"}
  {:from "config/alacritty"  :to "~/.config/alacritty"}
  {:from "config/tmux"       :to "~/.config/tmux"}
  {:from "config/emacs"      :to "~/.config/doom"}
  {:from "home/.tmux.conf"   :to "~/.tmux.conf"}
  {:from "home/.zshenv"      :to "~/.zshenv"}
  {:from "home/.gitconfig"   :to "~/.gitconfig"}]

 :machines
 {"yoshi" [{:from "config/hypr/machines/yoshi/monitors.conf"
             :to   "~/.config/hypr/monitors-machine.conf"}
            {:from "config/hypr/machines/yoshi/overrides.conf"
             :to   "~/.config/hypr/overrides-machine.conf"}]
  "brain" [{:from "config/hypr/machines/brain/monitors.conf"
             :to   "~/.config/hypr/monitors-machine.conf"}
            {:from "config/hypr/machines/brain/overrides.conf"
             :to   "~/.config/hypr/overrides-machine.conf"}]
  "walt"  [{:from "config/hypr/machines/walt/monitors.conf"
             :to   "~/.config/hypr/monitors-machine.conf"}]}}
```

---

## dots.bb — Symlink Manager

### Interface

```
bb dots link             — apply all links for this machine; skip valid existing; warn conflicts
bb dots status           — print table: ✓ active / ✗ missing / ⚠ conflict / → target path
bb dots unlink           — remove all managed symlinks (leaves originals untouched)
bb dots link :dry-run    — print what would change without applying
```

### Implementation Notes

- Read dotfiles dir from `(or (System/getenv "DOTFILES") (git-root))` — don't hardcode `/home/russ`
- Expand `~` in `:to` paths via `(System/getenv "HOME")`
- Detect hostname via `(str/trim (slurp "/etc/hostname"))`
- Use `babashka.fs`: `fs/sym-link?`, `fs/read-link`, `fs/create-sym-link`, `fs/delete`
- For a directory link (e.g. `config/zsh` → `~/.config/zsh`): link the whole dir, not individual files
- Conflict = target exists and is NOT a symlink pointing to the expected source → print warning, skip
- Machine links are merged on top of base `:links` after hostname resolution
- Exit code 1 if any conflict or broken link detected (useful for future CI / health checks)

### Status Output Format (example)

```
✓  config/foot       → /home/russ/.config/foot
✓  config/alacritty  → /home/russ/.config/alacritty
✗  config/nvim       → /home/russ/.config/nvim  (missing — run bb dots link)
⚠  home/.gitconfig   → /home/russ/.gitconfig    (conflict: not a symlink)
```

---

## bb.edn Updates

```clojure
{:tasks
 {:requires ([babashka.fs :as fs]
             [clojure.string :as str])

  say-it-aint-so (shell "echo 'it aint so'")
  doom-up        (shell "doom up -!")

  dots {:doc  "Symlink manager. Args: link | status | unlink | link :dry-run"
        :task (load-file "dots.bb")}}}
```

---

## Migration Phases

Each phase follows this checklist:
1. Move files to new location
2. Add/update entry in `dots.edn`
3. Run `bb dots link` — verify output is all ✓
4. Smoke-test the tool (open it, use it briefly)
5. Run `bb dots status` — confirm clean
6. Delete old stow package dir
7. Commit

---

### Phase 0 — Tooling (no file moves)

**Goal:** Get `dots.bb`, `dots.edn`, and updated `bb.edn` in place with zero entries.
Verify `bb dots status` runs cleanly before touching any configs.

Files:
- Create `dots.edn` (empty `:links []`)
- Create `dots.bb`
- Update `bb.edn`

Acceptance: `bb dots status` prints "No links configured." and exits 0.

---

### Phase 1 — foot (lowest risk)

foot has no shell integration; worst case is the terminal uses defaults.
alacritty remains as a working fallback terminal throughout this phase.

Steps:
- `mkdir -p config/foot`
- Move `foot/.config/foot/*` → `config/foot/`
- Add `{:from "config/foot" :to "~/.config/foot"}` to `dots.edn`
- `bb dots link` → verify ✓
- Open foot, confirm it loads with theme
- `rm -rf foot/`
- Commit: `dots: migrate foot to config/`

---

### Phase 2 — alacritty

Steps:
- `mkdir -p config/alacritty`
- Move `alacritty/.config/alacritty/*` → `config/alacritty/`
- Add entry to `dots.edn`
- `bb dots link` → verify ✓
- Open alacritty, confirm it loads
- `rm -rf alacritty/`
- Commit: `dots: migrate alacritty to config/`

---

### Phase 3 — git

Steps:
- `mkdir -p home`
- Move `git/.gitconfig` → `home/.gitconfig`
- Add `{:from "home/.gitconfig" :to "~/.gitconfig"}` to `dots.edn`
- `bb dots link` → verify ✓
- Run `git status` somewhere, confirm identity/config loads
- `rm -rf git/`
- Commit: `dots: migrate gitconfig to home/`

---

### Phase 4 — nvim

Steps:
- `mkdir -p config/nvim`
- Move `nvim/.config/nvim/*` → `config/nvim/`
- Move `nvim/.config/nvim.fnl/` → `config/nvim.fnl/` (if keeping the fennel parallel)
- Add entry to `dots.edn`
- `bb dots link` → verify ✓
- Open nvim, confirm plugins load (Lazy will reuse existing data dir)
- `rm -rf nvim/`
- Commit: `dots: migrate nvim to config/`

---

### Phase 5 — emacs / doom

Steps:
- `mkdir -p config/emacs`
- Move `emacs/.config/doom/*` → `config/emacs/`
- Add `{:from "config/emacs" :to "~/.config/doom"}` to `dots.edn`
- `bb dots link` → verify ✓
- Open emacs, confirm doom loads (may need `doom sync` if byte-compiled paths shift)
- `rm -rf emacs/`
- Commit: `dots: migrate doom emacs to config/`

**Watch out for:** doom stores compiled files in `~/.config/emacs/` (not `~/.config/doom/`).
The symlink only covers `~/.config/doom/` (the user config dir) — compiled artifacts
live separately and are unaffected.

---

### Phase 6 — hypr

hypr already has `setup_machine_config.bb` doing per-machine symlinks.
This phase absorbs that script into `dots.edn` `:machines` and deletes the script.

Steps:
- `mkdir -p config/hypr`
- Move all `hypr/*.conf` → `config/hypr/`
- Move `hypr/machines/` → `config/hypr/machines/`
- Add base entry + per-machine entries to `dots.edn`
- `bb dots link` → verify ✓ (includes machine-specific links)
- `hyprctl reload` → confirm Hyprland loads cleanly
- Remove `.local/bin/setup_machine_config.bb` (its job is now in dots.bb)
- `rm -rf hypr/`
- Commit: `dots: migrate hypr to config/, absorb machine symlinks`

---

### Phase 7 — tmux

tmux has two concerns: the config dir and `.tmux.conf` at `$HOME` root.
TPM bootstrap stays in `.tmux.conf` — do not change the plugin mechanism.

Steps:
- `mkdir -p config/tmux home`
- Move `tmux/.config/tmux/*` → `config/tmux/` (powerline theme, etc.)
- Move `tmux/.tmux.conf` → `home/.tmux.conf`
- Add entries to `dots.edn`
- `bb dots link` → verify ✓
- Open tmux, confirm TPM loads plugins (may need `prefix + I` first run)
- `rm -rf tmux/`
- Commit: `dots: migrate tmux to config/ + home/`

---

### Phase 8 — zsh (highest risk, do last)

**Before starting:** confirm all previous phases are stable and committed.
Have a fallback shell ready (fish, bash, or another open session).

Steps:
- `mkdir -p config/zsh home`
- Move `zsh/.config/zsh/*` → `config/zsh/`
- Move `zsh/.zshenv` → `home/.zshenv`
- Move `zsh/.profile` → `home/.profile` (add entry for it too)
- Add entries to `dots.edn`
- `bb dots link` → verify ✓
- Open a new zsh session, confirm antidote loads, aliases work, completions work
- `rm -rf zsh/`
- Commit: `dots: migrate zsh to config/ + home/`

**Watch out for:** `ZDOTDIR` — `.zshenv` sets this to `~/.config/zsh`. The symlink
must be in place before zsh starts or it won't find the rest of the config.

---

### Phase 9 — Archive dead WM configs

No symlinks involved. Pure directory moves.

Move to `_archive/`: `awesome/`, `i3/`, `sway/`, `chunkwm/`,
`compton/`, `picom/`, `rofi/`, `polybar/`, `i3-scrot/`

Do NOT move: `yabai/`, `hammerspoon/` (still used on macOS).

```bash
mkdir -p _archive
mv awesome i3 sway chunkwm compton picom rofi polybar i3-scrot _archive/
```

Commit: `archive: move dead WM configs to _archive/`

---

## Open Questions (for critique / follow-up sessions)

- [ ] Should `dots.edn` live at repo root or in `nixos/`? (Leaning: repo root — it's
      not NixOS-specific, used on macOS machines too)
- [ ] `DOTFILES` env var: set in `.zshenv`? or detect via `git rev-parse --show-toplevel`?
- [ ] Should `bb dots status` be wired into `dirty_repos.bb` as a health check?
- [ ] Other stow packages not covered above: `atuin/`, `clojure/`, `fennel/`, `lua/`,
      `nixpkgs/`, `notifications/`, `psql/`, `python/`, `ranger/`, `readline/`,
      `rip-grep/`, `screenshots/`, `waybar/` — migrate these too, or leave as stow?
- [ ] `home/` dir: any risk of collision with other tools expecting `~/home/`?
      (Probably fine — it's only a source dir, never symlinked whole)
- [ ] Walt (macOS) nix-darwin setup: separate plan session needed.

---

## Critique Checklist (for reviewing agents)

- [ ] Is the `dots.bb` interface sufficient, or are edge cases missing?
- [ ] Is directory-level linking (whole `config/zsh/` → `~/.config/zsh`) correct,
      or should it link individual files for finer control?
- [ ] Is the migration order safe? Any hidden dependencies between phases?
- [ ] Does Phase 8 (zsh) have an adequate fallback plan?
- [ ] Does the `:machines` key in `dots.edn` cover the `wordnerd` (no-nix macOS) case?
- [ ] Any other stow packages that should be in the migration list?
