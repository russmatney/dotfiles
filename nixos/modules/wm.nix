{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    # wms
    i3
    # sway

    # wm tools
    arandr
    rofi
    # variety
    feh
    polybarFull
    psmisc
    picom

    waybar
    wofi
    kitty
    kdePackages.dolphin
    grim
    slurp
    hyprpaper

    hyprlock
    hypridle
  ];

  # programs.waybar.enable = true;
  # programs.light.enable = true; # backlight support - requires user 'video' group

  # hypr
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
  };
  programs.uwsm.enable = true;

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # goes in home manager stuff :eyeroll:
  # wayland.windowManager.hyprland.plugins = with pkgs.hyprlandPlugins; [
  #   hyprwinwrap
  #   hyprexpo
  # ];

}
