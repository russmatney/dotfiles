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
    path = [ pkgs.powerline ];
    environment = {
        DISPLAY = ":0";
        SHELL = "${pkgs.zsh}/bin/zsh";
    };
    serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.tmux}/bin/tmux -v new-session -s debug -d";
        ExecStop = "${pkgs.tmux}/bin/tmux -v kill-session -t debug";
        KillMode = "process";
    };
  };

}
