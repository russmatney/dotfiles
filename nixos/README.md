# NixOS Configuration

## Flake-Based Configuration

This configuration now uses Nix flakes for better reproducibility and dependency management.

### Rebuilding the System

To rebuild your NixOS configuration with flakes:

```bash
# From the nixos directory
sudo nixos-rebuild switch --flake .#yoshi

# Or from anywhere in the dotfiles repo
sudo nixos-rebuild switch --flake /home/russ/dotfiles/nixos#yoshi
```

### Updating Dependencies

To update all flake inputs (nixpkgs, peon-ping, etc.):

```bash
cd /home/russ/dotfiles/nixos
nix flake update
```

To update just peon-ping:

```bash
cd /home/russ/dotfiles/nixos
nix flake lock --update-input peon-ping
```

### Installed via Flakes

- **peon-ping**: AI coding assistant notification system with voice lines from Warcraft, StarCraft, Portal, Zelda, and more

### Using peon-ping

After rebuilding, you can use:

```bash
# Check status
peon status

# Install sound packs
peon packs install peon
peon packs install glados
peon packs install sc_kerrigan

# List available packs
peon packs list
```

See the [peon-ping documentation](https://github.com/PeonPing/peon-ping) for integration with Claude Code and other AI tools.
