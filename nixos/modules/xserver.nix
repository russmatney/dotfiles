{ config, pkgs, lib, ... }:

{

  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;

    displayManager = {
      gdm.enable = true;
      # lightdm.enable = true;
      # defaultSession = "none+i3";
    };

    desktopManager = {
      gnome.enable = true;
    };

    # windowManager.i3.enable = true;

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
