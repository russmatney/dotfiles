# peon-ping → ECA integration
**Date:** 2026-04-25

Wired peon-ping (already installed via flake) into Claude Code hooks.

## What was done

- Created `~/.claude/hooks/peon-ping/` with symlinks to the nix store
- Wrote `~/.claude/settings.json` with full hooks block (SessionStart, Stop, Notification, PermissionRequest, PostToolUseFailure, UserPromptSubmit, etc.)
- Moved peon-ping package from inline flake snippet into `nixos/modules/editor.nix`
- Added `system.activationScripts.peonPingClaudeHooks` to recreate symlinks on every `nixos-rebuild switch` — keeps hooks pointing at current store path after updates

## Files changed

- `nixos/flake.nix` — removed inline peon-ping package block
- `nixos/modules/editor.nix` — added peon-ping-pkg + activation script

## Verify

```bash
peon status  # should show [x] Claude Code (installed)
```
