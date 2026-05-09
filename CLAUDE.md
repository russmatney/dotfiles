# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is Russ's personal dotfiles repository, managed with a custom Babashka-based symlink system called `dots`. The repository contains configuration files for a complete Linux development environment running NixOS with Hyprland window manager.

## Core Architecture

### Dots Symlink System
- Symlink manifest: `dots.edn` — declares all links as `{:from <repo-relative-path> :to <destination>}`
- Manager script: `dots.bb` — reads the manifest and applies/checks/removes symlinks
- Machine-specific links are declared under `:machines` keyed by hostname
- `:link-contents true` on an entry symlinks each file inside the directory individually (rather than the directory itself)

```bash
bb dots status        # print state of all declared links
bb dots link          # create missing symlinks
bb dots link --dry-run  # preview what would change
bb dots unlink        # remove all managed symlinks
```

### Key Configuration Areas

**NixOS System Configuration**
- Primary config: `nixos/configuration.nix`
- Modular structure in `nixos/modules/` with specialized modules for:
  - `clawe.nix`: Clawe WM integration with systemd services
  - `core.nix`: Base system configuration
  - `dev.nix`: Development tools
  - `wm.nix`: Window manager setup
  - `apps.nix`, `fonts.nix`, `security.nix`, etc.

**Hyprland Window Manager**
- Main config: `hypr/hyprland.conf` 
- Modular configuration files for different aspects:
  - `bindings.conf`: Key bindings
  - `monitors.conf`: Display configuration
  - `workspaces.conf`: Workspace management
  - `clients.conf`: Window rules
  - `look_and_feel.conf`: Styling

**Clawe Integration**
- Custom window management system built in Clojure/Babashka
- systemd services defined in `nixos/modules/clawe.nix`
- Services: doctor-backend, doctor-frontend, doctor-topbar, doctor-dashboard
- Working directory: `/home/russ/russmatney/clawe`

## Common Development Commands

### System Management
```bash
# Rebuild NixOS configuration
sudo nixos-rebuild switch

# Apply dotfile symlinks
bb dots link

# Check symlink status
bb dots status

# Restart Clawe services
systemctl --user restart doctor-backend
systemctl --user restart doctor-frontend
```

### Development Environment
```bash
# Source shell configuration
source ~/.config/zsh/.zshrc
# Or use alias: zz

# Tmux session management
tmux new-session -d -s <session-name>
tmux attach -t <session-name>
```

### Editor Configurations

**Emacs (Doom)**
- Configuration root: `emacs/.config/doom/`
- Main files: `init.el`, `config.el`, `packages.el`
- Custom modules: `+bindings.el`, `+hydra.el`, `+org-custom.el`, `+langs.el`
- Uses native compilation optimizations

**Neovim**
- Currently using AstroVim setup
- Configuration in `nvim/` directory

## Important Considerations

### File Visibility
- Many configurations use hidden (dot-prefixed) files
- Zsh configured with `setopt globdots` to show hidden files
- ripgrep configured with `~/.ignore` file containing `.git`

### Service Dependencies
- Clawe services depend on graphical session target
- Services expect Babashka (bb) to be available in PATH
- Environment variables like `DISPLAY=:0` required for GUI components

### Multi-Platform Support
- Primary target: NixOS/Linux
- Legacy macOS support in `osx/` directory
- Cross-platform shell configuration with conditional loading

### Development Workflow
- Org-mode files for todo management (`todo.org`)
- Git repository with active development
- Tmux for persistent terminal sessions
- Multiple monitor support with custom scripts

## Package Management

The system uses multiple package managers:
- **NixOS**: System-level packages and services
- **Antidote**: Zsh plugin management
- **Doom Emacs**: Emacs package management
- **tmux plugins**: Via tmux plugin manager

When adding new tools or configurations, prefer integration through the appropriate NixOS module rather than manual installation.