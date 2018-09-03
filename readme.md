# Stow-based Dotfiles

These files are structured by feature, and use GNU Stow for installation.

`stow foo` will create symlinks for everything in the `foo` dir
one directory above the folder you called it in.
This works well for dotfile management - you can create
feature-based directories in `~/dotfiles`, and then `stow feat`
to take care of dropping it into `~/.blah` for you.

For example, to install my `zsh` setup, you can:

```
hub clone russmatney/dotfiles ~/russ_dotfiles
cd russ_dotfiles
stow zsh
# note that you need to delete (backup) your .zsh* files beforehand
```

This is convenient for me, as I can `stow i3` on machines that need it.

Note that dependencies are not covered by `stow` - it is a simple
symlinking tool. See each module's readme for required software.
