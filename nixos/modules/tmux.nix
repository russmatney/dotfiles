{ config, lib, pkgs, ... }:

{
  # packages
  environment.systemPackages = with pkgs; [
    tmux
    powerline
  ];

  systemd.user.services.tmux = {
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    description = "tmux: A terminal multiplexer";
    environment = {
        DISPLAY = ":0";
        # SHELL = "/run/current-system/sw/bin/zsh";
        # SHELL = "/usr/bin/env zsh";
    };
    serviceConfig = {
        Type = "forking";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin:/home/russ/.local/bin";
        ExecStart = "${pkgs.tmux}/bin/tmux new -s debug -d";
        ExecStop = "${pkgs.tmux}/bin/tmux kill-server";
        # KillMode = "process";
    };
  };

}
