{ pkgs, config, ... }:

let
  sym = config.lib.file.mkOutOfStoreSymlink;
in {

  programs.home-manager.enable = true;
  home.stateVersion = "25.05";
  
  # dconf = {
  #   enable = true;
  # };

  # home.username = "russ";
  # home.homeDirectory = "/home/russ";

  # xsession.enable = true;

  # xdg.configFile = {
  home.file = { # and add ~/.config prefix
    ".config/atuin/config.toml".source = sym ../../atuin/.config/atuin/config.toml;

    # zsh
    ".config/zsh/.zshrc" = {
      source = sym ../../zsh/.config/zsh/.zshrc;
      # executable = true;
    };
    ".zshenv".source = sym ../../zsh/.zshenv;
    ".profile".source = sym ../../zsh/.profile;
    ".config/zsh/fzf.zsh" = {
      source = sym ../../zsh/.config/zsh/fzf.zsh;
      # executable = true;
    };
    ".config/zsh/fuzzy-sys.zsh" = {
      source = sym ../../zsh/.config/zsh/fuzzy-sys.zsh;
      # executable = true;
    };
    ".config/zsh/.zsh_plugins.txt" = {
      source = sym ../../zsh/.config/zsh/.zsh_plugins.txt;
    };
    ".config/zsh/grfn.zsh-theme" = {
      source = sym ../../zsh/.config/zsh/grfn.zsh-theme;
    };

    # tmux
    ".tmux.conf".source = sym ../../tmux/.tmux.conf;
    ".config/powerline/themes/tmux/default.json".source = sym ../../tmux/.config/powerline/themes/tmux/default.json;
    ".local/bin/start-tmux".source = sym ../../tmux/.local/bin/start-tmux;
    ".local/bin/tt".source = sym ../../tmux/.local/bin/tt;

  };

}
