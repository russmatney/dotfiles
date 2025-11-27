{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      maestral
      maestral-gui
    ];
  };

  systemd.user.services.maestral = {
    description = "Maestral daemon";

    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      Type = "notify";
      NotifyAccess = "exec";
      ExecStart = "${pkgs.maestral}/bin/maestral start -f";
      ExecStop = "${pkgs.maestral}/bin/maestral stop";
      ExecStopPost = ''
        ${pkgs.bash}/bin/bash -c "if [ $SERVICE_RESULT != success ]; then \
        ${pkgs.libnotify}/bin/notify-send Maestral 'Daemon failed'; fi"
      '';
      WatchdogSec = "30s";
    };
  };
}
