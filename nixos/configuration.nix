# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix

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

  networking.hostName = "yoshi";

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}

