# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  # Determine hostname - reads from /etc/hostname or falls back to "yoshi"
  hostname = builtins.readFile /etc/hostname;
  # # Strip whitespace/newlines
  # cleanHostname = builtins.replaceStrings ["\n" " "] ["" ""] hostname;

  # Machine-specific configuration path
  machineConfig = ./machines + "/${hostname}.nix";
in
{
  imports = [
    # Import machine-specific configuration (includes hardware-configuration.nix)
    machineConfig

    ./modules/user.nix

    ./modules/apps.nix
    ./modules/clawe.nix
    ./modules/core.nix
    ./modules/dev.nix
    ./modules/dropbox.nix
    ./modules/editor.nix
    ./modules/fonts.nix
    ./modules/keyboard.nix
    ./modules/security.nix
    ./modules/tmux.nix
    ./modules/wm.nix
    ];


  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}

