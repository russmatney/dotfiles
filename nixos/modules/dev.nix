{ pkgs, config, ...}:

{
  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  programs.git.enable = true;
  programs.zsh.enable = true;

  services.udisks2.enable = true;
  services.devmon.enable = true;
  services.gvfs.enable = true;

  environment.variables = {
      ANTIDOTE_PATH = "${pkgs.antidote}/share/antidote/antidote.zsh";
  };

  # packages
  environment.systemPackages = with pkgs; [
    # nix
    home-manager

    # cli
    git
    wget
    stow
    xclip # required for neovim paste
    gcc
    unzip
    htop
    fd
    jq
    tldr

    # notifs
    dunst
    libnotify

    # zsh
    antidote
    # zsh pkg mgr

    # fancy cli
    atuin # <C-r>
    direnv
    eza # ls
    fzf
    gitAndTools.git-extras
    gitAndTools.gh
    procs # ps
    ripgrep # rg

    # util
    babashka
    pandoc
    shellcheck
    sqlite
    playerctl
    acpi
    acpid
    inotify-tools
    brightnessctl

    # ai
    aider-chat-full
    aichat

    # terminals
    alacritty
    # console

    # langs
    clojure
    nodejs_24
    python3Full
    yarn

    copilot-language-server-fhs
  ];
}
