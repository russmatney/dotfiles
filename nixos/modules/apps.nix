{ config, lib, pkgs, ... }:

{
  # firefox
  programs.firefox.enable = true;

  # ff and chromium
  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
     enableGoogleTalkPlugin = true;
     enableAdobeFlash = true;
    };

    # chromium = {
    #  enablePepperFlash = true; # Chromium removed support for Mozilla (NPAPI) plugins so Adobe Flash no longer works
    # };
  };

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

    # browsers
    # firefoxWrapper
    chromium

    # video
    obs-studio
    kdePackages.kdenlive
  ];
}
