{ config, lib, pkgs, ... }:

{

  # keyboard options
  # surpassed by hyprland config
  services.xserver = {
    autoRepeatDelay = 150;
    autoRepeatInterval = 80;

    # Configure keymap in X11
    xkb = {
      layout = "us";
      variant = "";
      options = "caps:escape";
    };
  };

  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "escape";
          };
        };
      };
    };
  };

  hardware.keyboard.zsa.enable = true;

  environment.systemPackages = with pkgs; [
    wally-cli
    keymapp
  ];

}
