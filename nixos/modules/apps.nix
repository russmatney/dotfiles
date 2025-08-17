{ config, lib, pkgs, ... }:

{

  # programs
  programs.firefox.enable = true;
  programs.obs-studio = {
    enable = true;
    enableVirtualCamera = true;
  };

  # packages
  environment.systemPackages = with pkgs; [
    # chat
    discord
    slack

    # music
    spotify

    # todo
    ticktick
    obsidian

    # gamedev
    godot
    aseprite

    # video
    obs-studio
    kdePackages.kdenlive
  ];
}
