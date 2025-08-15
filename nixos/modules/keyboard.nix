{ config, lib, pkgs, ... }:

{

  # keyboard options
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


}
