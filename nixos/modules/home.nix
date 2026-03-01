{ ... }: {
  programs.home-manager.enable = true;

  wayland.windowManager.hyprland.systemd.enable = false;

  home.stateVersion = "25.11";
}
