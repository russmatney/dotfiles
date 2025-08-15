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
    polybar
    psmisc
    picom

  ];

  programs.sway.enable = true;
  programs.waybar.enable = true;

  services.displayManager.defaultSession = "none+i3";
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # display manager
  services.xserver.displayManager = {
    # lightdm = {
    #   enable = true;
    #   greeter.enable = true;
    # };
    gdm.enable = true;
    gdm.banner = ''WHAT UP'';
  };

  # desktop and window managers
  services.xserver = {
    desktopManager.gnome.enable = true;
    desktopManager.xterm.enable = true;
    windowManager.i3.enable = true;
    windowManager.bspwm.enable = true;
  };

}
