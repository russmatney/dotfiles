# Machine-Specific Configuration

This dotfiles repository supports machine-specific configurations to customize settings per machine.

## Machines

Current machines in this setup:
- **yoshi** - Primary development workstation (Framework 16-inch AMD)
- **walt** - [Define role]
- **wordnerd** - [Define role]
- **brain** - [Define role]

## Architecture

### NixOS Configuration

Machine-specific NixOS modules are in `nixos/machines/`:
```
nixos/machines/
├── yoshi.nix       # Yoshi-specific packages, services, settings
├── walt.nix
├── wordnerd.nix
└── brain.nix
```

The main `nixos/configuration.nix` automatically imports the correct machine config based on hostname.

**What to put in machine configs:**
- Machine-specific packages
- Hardware-specific services
- Performance tuning (CPU governor, power management)
- Machine role environment variables

### Hyprland Configuration

Machine-specific Hyprland configs are in `hypr/machines/<hostname>/`:
```
hypr/machines/
├── yoshi/
│   ├── monitors.conf   # Monitor layout and configuration
│   └── overrides.conf  # Keybindings, window rules, etc.
├── walt/
├── wordnerd/
└── brain/
```

**What to put in machine configs:**
- `monitors.conf` - Monitor layouts, resolutions, scaling
- `overrides.conf` - Machine-specific keybindings, window rules, performance settings

## Setup Instructions

### First Time Setup

1. Ensure hostname is set correctly in `/etc/hostname` (done via NixOS)

2. Run the machine config setup script:
```bash
~/.local/bin/setup_machine_config.bb
```

This creates symlinks:
- `~/.config/hypr/monitors-machine.conf` → `~/dotfiles/hypr/machines/<hostname>/monitors.conf`
- `~/.config/hypr/overrides-machine.conf` → `~/dotfiles/hypr/machines/<hostname>/overrides.conf`

3. Rebuild NixOS:
```bash
sudo nixos-rebuild switch
```

4. Reload Hyprland:
```bash
hyprctl reload
# or: SUPER+R
```

### Adding a New Machine

1. Create NixOS config:
```bash
cp nixos/machines/yoshi.nix nixos/machines/newmachine.nix
# Edit newmachine.nix with machine-specific settings
```

2. Create Hyprland configs:
```bash
mkdir -p hypr/machines/newmachine
touch hypr/machines/newmachine/monitors.conf
touch hypr/machines/newmachine/overrides.conf
# Edit configs for the new machine
```

3. Set hostname in NixOS configuration.nix:
```nix
networking.hostName = "newmachine";
```

4. Run setup script and rebuild

## Best Practices

### What Should Be Machine-Specific

**YES - Put in machine configs:**
- Monitor configurations (different display setups)
- Hardware-specific packages (laptop vs desktop tools)
- Performance settings (power management, GPU settings)
- Machine-specific keybindings (media keys, special hardware)
- Role-specific applications (server tools, creative software)

**NO - Keep in shared configs:**
- Core keybindings and workflows
- Standard application configurations
- Theme and styling preferences
- General development tools

### Development Workflow

When testing machine-specific changes:

1. Edit the machine config files directly
2. For NixOS changes: `sudo nixos-rebuild switch`
3. For Hyprland changes: `hyprctl reload`
4. No need to rerun setup script unless adding new files

## Troubleshooting

**Hyprland won't start:**
- Check that symlinks exist: `ls -la ~/.config/hypr/*machine.conf`
- Rerun setup script: `~/.local/bin/setup_machine_config.bb`

**Wrong machine config loading:**
- Check hostname: `hostname` and `cat /etc/hostname`
- Verify machine config exists: `ls ~/dotfiles/nixos/machines/<hostname>.nix`

**Changes not applying:**
- NixOS: Run `sudo nixos-rebuild switch`
- Hyprland: Run `hyprctl reload` or restart Hyprland
