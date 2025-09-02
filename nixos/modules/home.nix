{ pkgs, config, ... }:

let
  sym = config.lib.file.mkOutOfStoreSymlink;
in {
  programs.home-manager.enable = true;

  wayland.windowManager.hyprland.systemd.enable = false;

  home.stateVersion = "25.05";

  home.file = { # and add ~/.config prefix
    ".config/atuin/config.toml".source = sym ../../atuin/.config/atuin/config.toml;

    # zsh
    ".config/zsh/.zshrc" = {
      source = sym ../../zsh/.config/zsh/.zshrc;
    };
    ".zshenv".source = sym ../../zsh/.zshenv;
    ".profile".source = sym ../../zsh/.profile;
    ".config/zsh/fzf.zsh" = {
      source = sym ../../zsh/.config/zsh/fzf.zsh;
    };
    ".config/zsh/fuzzy-sys.zsh" = {
      source = sym ../../zsh/.config/zsh/fuzzy-sys.zsh;
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
    ".local/bin/set-monitor-config".source = sym ../../i3/.local/bin/set-monitor-config;

    ".config/polybar/launch.sh".source = sym ../../polybar/.config/polybar/launch.sh;
    ".config/polybar/config.ini".source = sym ../../polybar/.config/polybar/config.ini;
    ".config/polybar/scripts/info-hackspeed.sh".source = sym ../../polybar/.config/polybar/scripts/info-hackspeed.sh;
    ".config/polybar/scripts/isactive-bluetooth.sh".source = sym ../../polybar/.config/polybar/scripts/inactive-bluetooth.sh;
    ".config/polybar/scripts/openweather-fullfeatured.sh".source = sym ../../polybar/.config/polybar/scripts/openweather-fullfeatured.sh;
    ".config/polybar/scripts/spotify_status.py".source = sym ../../polybar/.config/polybar/scripts/spotify_status.py;

    # sway
    # NOTE not tested/working, just roughed out
    ".config/sway/config".source = sym ../../sway/.config/sway/config;
    ".config/sway/config.d/russ.conf".source = sym ../../sway/.config/sway/config.d/russ.conf;
    # ".config/waybar/config.jsonc".source = sym ../../sway/.config/waybar/config.jsonc;

    # alacritty
    ".config/alacritty/alacritty.toml".source = sym ../../alacritty/.config/alacritty/alacritty.toml;
    ".config/alacritty/alacritty.linux.toml".source = sym ../../alacritty/.config/alacritty/alacritty.linux.toml;

    # picom
    ".config/picom/picom.conf".source = sym ../../picom/.config/picom/picom.conf;

    # rofi
    ".config/rofi/config".source = sym ../../rofi/.config/rofi/config;
    ".config/rofi/config.rasi".source = sym ../../rofi/.config/rofi/config.rasi;
    ".config/rofi/slate.rasi".source = sym ../../rofi/.config/rofi/slate.rasi;

    # clawe
    ".local/bin/clawebb".source = sym ../../clawe/.local/bin/clawebb;
    ".local/bin/clawebb-log".source = sym ../../clawe/.local/bin/clawebb-log;

    # hypr things
    # ".config/waybar/config.jsonc".source = sym ../../waybar/config.jsonc;

  };

}
