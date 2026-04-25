# AGENTS.md

Guidance for AI coding agents operating in this dotfiles repository.
See `CLAUDE.md` for full architecture and context.

## Key Commands

```bash
sudo nixos-rebuild switch --flake /home/russ/dotfiles/nixos#yoshi  # apply NixOS changes
stow <dir>          # symlink a config package into $HOME
bb <task>           # run a Babashka task (see bb.edn)
systemctl --user restart doctor-backend  # restart Clawe services
hyprctl reload      # reload Hyprland config
```

There are no unit tests and no CI. "Testing" a change means rebuilding and observing behavior.

## Code Style

**Nix** — 2-space indent; `with pkgs; [ ... ]` for package lists; group imports logically
(user → apps → clawe → core → dev). Machine-specific config goes in `nixos/machines/<hostname>.nix`.

**Babashka/Clojure** (`.local/bin/*.bb`) — kebab-case functions; predicate names end in `?`;
`deps/add-deps` before `require`; clawe dependency expected at `/home/russ/russmatney/clawe`;
use `r.hypr/notify` for user-visible errors, `try/catch Exception` for external calls.

**Shell scripts** — double-quote all variable expansions; use `jq` for JSON (e.g. `hyprctl ... -j | jq`);
no `set -e` convention. Shebang: `#!/usr/bin/env bash` or `#!/run/current-system/sw/bin/bb`.

**Lua** (Neovim) — 2-space indent; `---@type LazySpec` annotation on plugin specs;
each file in `nvim/.config/nvim/lua/plugins/` returns one table.

**Elisp** (Doom Emacs) — `;;; file.el -*- lexical-binding: t; -*-` header required;
2-space indent; use `comment` macro for REPL scratch blocks.

## Sessions & Planning

AI-assisted planning lives in `.claude/skills/` and `sessions/`.

**`sessions/`** is gitignored — personal notes, never committed. When arriving on a
new machine, the dir will be empty; run the `braindump` skill to rebuild context.

**Core skills:**

| Skill       | When to use                                               |
|-------------|-----------------------------------------------------------|
| `braindump` | Starting a new planning session or after a life change    |
| `yodo`      | Quick "what should I do next?" based on past sessions     |

**To load a skill**, tell the agent: _"load the braindump skill"_ or _"load yodo"_.

**Workflow on a new machine:**
1. Clone dotfiles, run stow / nix setup as normal
2. Run `braindump` skill to capture current state into `sessions/`
3. Run `yodo` any time you want prioritized next steps

**Adding a session manually:**
- Create `sessions/YYYY-MM-DD-<slug>.md`
- Follow the structure in any existing session file (Life Context → Devices → Daily
  Tools → Key Systems → Pain Points → Desired Outcomes → Open Questions → Next Steps)

## Package Management

Always add new tools through NixOS modules — never install manually:
- System/CLI tools → `nixos/modules/dev.nix`
- GUI apps → `nixos/modules/apps.nix`
- Machine-specific → `nixos/machines/<hostname>.nix`
