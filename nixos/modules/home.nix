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

    # i3
    ".config/i3/config".source = sym ../../i3/.config/i3/config;

    ".config/polybar/launch.sh".source = sym ../../polybar/.config/polybar/launch.sh;
    ".config/polybar/config.ini".source = sym ../../polybar/.config/polybar/config.ini;
    ".config/polybar/scripts/info-hackspeed.sh".source = sym ../../polybar/.config/polybar/scripts/info-hackspeed.sh;
    ".config/polybar/scripts/isactive-bluetooth.sh".source = sym ../../polybar/.config/polybar/scripts/inactive-bluetooth.sh;
    ".config/polybar/scripts/openweather-fullfeatured.sh".source = sym ../../polybar/.config/polybar/scripts/openweather-fullfeatured.sh;
    ".config/polybar/scripts/spotify_status.py".source = sym ../../polybar/.config/polybar/scripts/spotify_status.py;

    # alacritty
    ".config/alacritty/alacritty.toml".source = sym ../../alacritty/.config/alacritty/alacritty.toml;
    ".config/alacritty/alacritty.linux.toml".source = sym ../../alacritty/.config/alacritty/alacritty.linux.toml;

    # picom
    ".config/picom/picom.conf".source = sym ../../picom/.config/picom/picom.conf;

    # clawe
    ".local/bin/clawebb".source = sym ../../clawe/.local/bin/clawebb;
    ".local/bin/clawebb-log".source = sym ../../clawe/.local/bin/clawebb-log;

  };

}
