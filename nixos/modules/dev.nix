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
    wl-clipboard-rs
    gcc
    unzip
    htop
    fd
    jq
    tldr
    framework-tool

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
    lazygit
    delta

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
    imagemagick
    trashy
    mermaid-cli

    # ai
    aider-chat-full
    aichat
    copilot-language-server-fhs

    # terminals
    alacritty
    # console

    # langs
    clojure
    nodejs_24
    yarn
    python3Full
    python313Packages.pip
    luajitPackages.luarocks
    go
    ruby
    rustup
  ];
}
