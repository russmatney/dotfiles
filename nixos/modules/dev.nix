{ pkgs, config, ...}:

{

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # programs
  programs.firefox.enable = true;
  programs.git.enable = true;
  programs.zsh.enable = true;

  # packages
  environment.systemPackages = with pkgs; [
    # nix
    home-manager

    # cli
    git
    gitAndTools.git-extras
    gitAndTools.gh
    wget
    atuin # <C-r>
    tmux
    powerline
    fzf
    httpie # curl
    stow
    ripgrep # ag/grep
    eza # ls
    antidote # zsh pkg mgr
    pandoc
    shellcheck
    sqlite
    xclip # required for neovim paste
    direnv
    procs # ps

    # supporting misc lsps and things
    gcc
    unzip
    nodejs_24
    # dotnet-sdk
    # dotnet-runtime

    # wm
    i3

    # wm tools
    rofi
    variety
    alacritty
    polybar
    psmisc
    picom

    # editor
    vim
    neovim
    neovide
    nodePackages.prettier
    emacs

    # python things
    python3
    # (python313.withPackages (pythonPackages: with pythonPackages; [
    #     maestral-gui
    # ]))

    # clj
    babashka

    # gamedev
    godot
    aseprite

    # apps
    discord
    slack
    spotify
    ticktick

    # clawe bb deps
    clojure
    libnotify

  ];

  services.lorri.enable = true;

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

  services.emacs = {
      enable = true;
      package = pkgs.emacs;
  };

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
