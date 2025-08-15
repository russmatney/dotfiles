{ config, lib, pkgs, ... }:

{

  systemd.user.services.doctor-backend = {
    wantedBy = [ "default.target" ];
    description = "clawe/doctor-backend: A server for clawe things";
    path = [ pkgs.zsh pkgs.bash pkgs.clojure pkgs.libnotify ];
    environment = {
        DISPLAY = ":0";
    };
    serviceConfig = {
        Type = "simple";
        # Environment = "PATH=/run/current-system/sw/bin";
        ExecStart = "${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn doctor-be";
        KillMode = "process";
    };
  };

  systemd.user.services.doctor-frontend = {
    wantedBy = [ "default.target" ];
    description = "clawe/doctor-frontend: Web views for clawe things";
    path = [ pkgs.zsh pkgs.bash pkgs.clojure pkgs.libnotify ];
    environment = {
        DISPLAY = ":0";
    };
    serviceConfig = {
        Type = "simple";
        # Environment = "PATH=/home/russ/.local/bin:/home/russ/.nix-profile/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin";
        ExecStart = "${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn doctor-fe";
        KillMode = "process";
    };
  };

  systemd.user.services.doctor-topbar = {
    wantedBy = [ "default.target" ];
    description = "clawe/doctor-topbar: A wanna-be ClaweWM bar";
    path = [ pkgs.zsh pkgs.bash pkgs.clojure ];
    environment = {
        DISPLAY = ":0";
        WEBKIT_DISABLE_DMABUF_RENDERER = "1";
    };
    serviceConfig = {
        Type = "simple";
        # Environment = "PATH=/home/russ/.local/bin:/home/russ/.nix-profile/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin";
        ExecStart = "${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn doctor-topbar";
        KillMode = "process";
    };
  };

  systemd.user.services.doctor-dashboard = {
    wantedBy = [ "default.target" ];
    description = "clawe/doctor-dashboard: A client for doctor's main webview";
    path = [ pkgs.zsh pkgs.bash pkgs.clojure ];
    environment = {
        DISPLAY = ":0";
        WEBKIT_DISABLE_DMABUF_RENDERER = "1";
    };
    serviceConfig = {
        Type = "simple";
        # Environment = "PATH=/home/russ/.local/bin:/home/russ/.nix-profile/bin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin";
        ExecStart = "${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn doctor-dashboard";
        KillMode = "process";
    };
  };

}
