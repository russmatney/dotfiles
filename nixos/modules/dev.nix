{ pkgs, config, ...}:

{
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

    gcc
    unzip
    nodejs_24
    # dotnet-sdk
    # dotnet-runtime

    # editor
    vim
    neovim
    neovide
    nodePackages.prettier
    emacs

    # wm
    i3

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
  ];

  services.lorri.enable = true;

  systemd.user.services.tmux = {
    wantedBy = [ "default.target" ];
    description = "tmux: A terminal multiplexer";
    serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.tmux}/bin/tmux new-session -s debug -d";
        ExecStop = "${pkgs.tmux}/bin/tmux kill-session -t debug";
        KillMode = "process";
    };
  };

  services.emacs = {
      enable = true;
      package = pkgs.emacs;
  };

}
