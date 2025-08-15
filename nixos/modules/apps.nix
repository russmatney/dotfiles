{ config, lib, pkgs, ... }:

{

  # programs
  programs.firefox.enable = true;

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
  ];
}
