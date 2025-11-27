{ pkgs, config, ... }:

let
  sym = config.lib.file.mkOutOfStoreSymlink;
in {
  programs.home-manager.enable = true;

  wayland.windowManager.hyprland.systemd.enable = false;

  home.stateVersion = "25.05";
}
