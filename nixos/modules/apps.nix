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

  xdg.mime.defaultApplications = {
    "text/html" = "org.firefox.desktop";
    "x-scheme-handler/http" = "org.firefox.desktop";
    "x-scheme-handler/https" = "org.firefox.desktop";
    "x-scheme-handler/about" = "org.firefox.desktop";
    "x-scheme-handler/unknown" = "org.firefox.desktop";
  };

  # programs.obs-studio = {
  #   enable = true;
  #   enableVirtualCamera = true;
  # };

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
    # godot
    godotPackages_4_5.godot
    aseprite

    # browsers
    # firefoxWrapper
    chromium

    # video
    # obs-studio
    kdePackages.kdenlive

    # password
    _1password-gui
    _1password-cli

    # ai
    lmstudio
  ];
}
