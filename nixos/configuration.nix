# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix

    # home manager
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-25.05.tar.gz}/nixos")

    ./modules/apps.nix
    ./modules/clawe.nix
    ./modules/core.nix
    ./modules/dev.nix
    ./modules/dropbox.nix
    ./modules/editor.nix
    ./modules/keyboard.nix
    ./modules/security.nix
    ./modules/tmux.nix
    ./modules/wm.nix

    ];

  networking.hostName = "yoshi";

  nixpkgs.config.allowUnfree = true;

  # users
  users = {
    # mutableUsers = false;
    # users.default = {
    #   home = "/home/${toString config.users.users.default.name}";
    #   extraGroups = [ "networkmanager" "wheel" ];
    #   shell = pkgs.zsh;
    #   isNormalUser = true;
    # };
    users.russ = {
      home = "/home/russ";
      extraGroups = [ "networkmanager" "wheel" ];
      shell = pkgs.zsh;
      isNormalUser = true;
    };
  };
  home-manager = {
    useUserPackages = true;
    # users.default = import ./modules/home.nix;
    users.russ = import ./modules/home.nix;
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}

