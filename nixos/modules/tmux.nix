{ config, lib, pkgs, ... }:

{
  # packages
  environment.systemPackages = with pkgs; [
    tmux
    powerline
  ];

  systemd.user.services.tmux = {
    wantedBy = [ "default.target" ];
    description = "tmux: A terminal multiplexer";
    environment = {
        DISPLAY = ":0";
        SHELL = "/usr/bin/env zsh";
    };
    serviceConfig = {
        Type = "forking";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin";
        ExecStart = "${pkgs.tmux}/bin/tmux -v new-session -s debug -d";
        ExecStop = "${pkgs.tmux}/bin/tmux -v kill-session -t debug";
        KillMode = "process";
    };
  };

}
