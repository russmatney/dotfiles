{ config, pkgs, lib, ... }:

{

  services.displayManager.defaultSession = "none+i3";
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # display manager
  services.xserver.displayManager = {
    session = [];

    # lightdm = {
    #   enable = true;
    #   greeter.enable = true;
    # };
    gdm.enable = true;
    gdm.banner = ''WHAT UP'';
  };

  services.xserver.desktopManager.gnome.flashback.customSessions = [
    {
      wmName = "xmonad";
      wmLabel = "XMonad";
      wmCommand = "${pkgs.haskellPackages.xmonad}/bin/xmonad";
      enableGnomePanel = false;
    }
    {
      wmName = "i3";
      wmLabel = "i3";
      wmCommand = "${pkgs.i3}/bin/i3";
      enableGnomePanel = false;
    }
  ];

  # desktop and window managers
  services.xserver = {
    desktopManager.gnome.enable = true;
    desktopManager.xterm.enable = true;
    windowManager.i3.enable = true;
    windowManager.bspwm.enable = true;
  };

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
