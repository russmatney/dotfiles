# [Zsh](https://github.com/zsh-users/zsh)

```
brew install zsh zsh-completions
```

See `zshrc` for full config.

# [Tmux](https://github.com/tmux/tmux)

```
brew install tmux
```

See `tmux.conf` for full config.

## `tt` and sessions

See `zsh/tmux.aliases.zsh`

## [Powerline](https://github.com/powerline/powerline)

A beautiful tmux status line.

```
pip install powerline-status
```

# [Emacs](https://emacsformacosx.com/)

It's useful to have both a GUI (link above) and CLI emacs version.

```
brew install emacs
```

`doom-russ/*` is my private module for running [Doom Emacs](https://github.com/hlissner/doom-emacs) by `hlissner`

See `doom-russ/+{bindings,+leader,+commands}.el` for bindings and commands.

# Vim ([neovim](https://github.com/neovim/neovim))

```
brew tap neovim/neovim
brew install neovim
# or
pip install neovim
```

See `nvimrc` for full config.

For plugins, use [vim-plug](https://github.com/junegunn/vim-plug) and my list at `nvim/plugins.vim`.

# CLI

## [httpie](https://github.com/jakubroztocil/httpie)

A better `curl`.

```
brew install httpie
```

## [fzf](https://github.com/junegunn/fzf)

Fuzzy-search for history + file finding.

```
brew install fzf
```

## [autojump/j](https://github.com/wting/autojump)

`j <search>` to hop to a matching dir anywhere in your file system.

```
brew install autojump
```

# OSX

For general OSX fixes, see `osx/osx-hacks.sh`.

## [iTerm2](https://iterm2.com/downloads/nightly/#/section/home)

- [Set zsh as default shell](http://stackoverflow.com/questions/13476232/make-iterm2-launch-with-zsh)
- Turn off Lion-style full screen windows

## [Homebrew](http://brew.sh/)

```
brew install coreutils
brew install caskroom/cask/brew-cask
```

## [Alfred](https://www.alfredapp.com/)

A spotlight replacement with hackable OSX workflows.

Some workflows:

- [dash](https://github.com/Kapeli/Dash-Alfred-Workflow) - Easily my most used,
  if not the whole reason to get Alfred (and Dash) in the first place. Faster
  than Googling for docs for any language.
- [top (including kill)](http://zhaocai.github.io/alfred2-top-workflow/) - because the activity monitor is just too slow, especially when something needs to DIE
- [github](https://github.com/gharlan/alfred-github-workflow) - `gh [search|repo|etc]`
- [ip](http://dferg.us/ip-address-workflow/) - quick local/external ip lookup

## [Dash Docs](https://kapeli.com/dash)

A great pair for Alfred, Dash is a generic offline docs tool.

## [Hammerspoon](http://www.hammerspoon.org/)

Shortcuts for your mac. Used for fast app switching.

```lua
-- in ~/.hammerspoon/init.lua

function appShortcut(modifier, character, application)
  hs.hotkey.bind(modifier, character, function() hs.application.launchOrFocus(application) end)
end

local alt = {'alt'}

appShortcut(alt, 'C', 'Google Chrome')
appShortcut(alt, 'T', 'iTerm')
appShortcut(alt, 'P', 'Spotify')
appShortcut(alt, 'M', 'Messages')
appShortcut(alt, 'N', 'nvALT')
appShortcut(alt, 'A', 'Activity Monitor')
appShortcut(alt, 'L', 'Slack')
appShortcut(alt, 'D', 'Dash')
```

## [BetterSnapTool](https://itunes.apple.com/us/app/bettersnaptool/id417375580?mt=12)

Window movement and resizing via the keyboard.

## [Vimium (Chrome Extension)](https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb?hl=en)

Vim bindings for navigating Chrome.

