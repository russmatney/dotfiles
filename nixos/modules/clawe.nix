{ config, lib, pkgs, ... }:

{

  # bb nrepl
  systemd.user.services.bb-nrepl = {
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    description = "babashka nrepl: running on 1337";
    environment = {};
    serviceConfig = {
        Type = "simple";
        WorkingDirectory = "/home/russ/dotfiles";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin";
        ExecStart = "${pkgs.runtimeShell} -c 'source ${config.system.build.setEnvironment}; ${pkgs.babashka}/bin/bb --nrepl-server 1337'";
    };
  };

  systemd.user.services.doctor-backend = {
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    description = "clawe/doctor-backend: A server for clawe things";
    environment = {
        DISPLAY = ":0";
    };
    serviceConfig = {
        Type = "simple";
        WorkingDirectory = "/home/russ/russmatney/clawe";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin";
        ExecStart = "${pkgs.runtimeShell} -c 'source ${config.system.build.setEnvironment}; ${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn doctor-be'";
    };
  };

  systemd.user.services.doctor-frontend = {
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    description = "clawe/doctor-frontend: Web views for clawe things";
    environment = {
        DISPLAY = ":0";
    };
    serviceConfig = {
        Type = "simple";
        WorkingDirectory = "/home/russ/russmatney/clawe";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin";
        ExecStart = "cd /home/russ/russmatney/clawe && ${pkgs.babashka}/bin/bb doctor-fe";
    };
  };

  systemd.user.services.doctor-topbar = {
    # wantedBy = [ "graphical-session.target" ];
    # after = [ "graphical-session.target" ];
    description = "clawe/doctor-topbar: A wanna-be ClaweWM bar";
    path = [ pkgs.zsh pkgs.bash pkgs.clojure ];
    environment = {
        DISPLAY = ":0";
        WEBKIT_DISABLE_DMABUF_RENDERER = "1";
    };
    serviceConfig = {
        Type = "simple";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin:/home/russ/.cargo/bin";
        ExecStart = "${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn topbar";
    };
  };

  systemd.user.services.doctor-dashboard = {
    # wantedBy = [ "graphical-session.target" ];
    # after = [ "graphical-session.target" ];
    description = "clawe/doctor-dashboard: A client for doctor's main webview";
    path = [ pkgs.zsh pkgs.bash pkgs.clojure ];
    environment = {
        DISPLAY = ":0";
        WEBKIT_DISABLE_DMABUF_RENDERER = "1";
    };
    serviceConfig = {
        Type = "simple";
        Environment = "PATH=/run/wrappers/bin:/run/current-system/sw/bin:/home/russ/.cargo/bin";
        ExecStart = "${pkgs.babashka}/bin/bb --config /home/russ/russmatney/clawe/bb.edn dashboard";
    };
  };

}
