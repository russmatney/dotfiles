# Overview

- Zsh
- Tmux
- Emacs
- Vim
- CLI
  - httpie
  - fzf
  - autojump
- OSX
  - iTerm2
  - Homebrew
  - Alfred
  - Dash Docs
  - Hammerspoon
  - BetterSnapTool
  - Vimium (Chrome Extension)

# Zsh

```
brew install zsh zsh-completions
```

See `zshrc` for full config.

# Tmux

```
brew install tmux
```

See `tmux.conf` for full config.

## `tt` and sessions

See `zsh/tmux.aliases.zsh`

## Powerline

[Powerline source](https://github.com/powerline/powerline).

```
pip install powerline-status
```

# Emacs

`doom-russ` is my private module for running [Doom Emacs](https://github.com/hlissner/doom-emacs) by `hlissner`

See `doom-russ/+{bindings,+leader,+commands}.el` for bindings and commands.

# Vim

```
brew tap neovim/neovim
brew install neovim
# or
pip install neovim
```

See `nvimrc` for full config.

## Plugins: use vim-plug

# CLI

## httpie

```
brew install httpie
```

## fzf

```
brew install fzf
```

## autojump

```
brew install autojump
```

# OSX

For general OSX fixes, see `osx/osx-hacks.sh`.

## iTerm2

[iTerm2 Installation](https://iterm2.com/downloads/nightly/#/section/home)

Configuration:

  - [Set zsh as default shell](http://stackoverflow.com/questions/13476232/make-iterm2-launch-with-zsh)
  - Turn off Lion-style full screen windows

## Homebrew

[Homebrew](http://brew.sh/)

  - `brew install coreutils`
  - `brew install caskroom/cask/brew-cask` - generic req for brew casks

## Alfred

[Alfred](https://www.alfredapp.com/)
is a Spotlight replacement with hackable OSX workflows.

### Alfred Workflows:

- [dash](https://github.com/Kapeli/Dash-Alfred-Workflow) - Easily my most used,
  if not the whole reason to get Alfred (and Dash) in the first place. Faster
  than Googling for docs for any language.
- [top (including kill)](http://zhaocai.github.io/alfred2-top-workflow/) - because the activity monitor is just too slow, especially when something needs to DIE
- [github](https://github.com/gharlan/alfred-github-workflow) - `gh [search|repo|etc]`
- [ip](http://dferg.us/ip-address-workflow/) - quick local/external ip lookup

## Dash Docs

A great pair for Alfred, Dash is a generic offline docs tool.

## Hammerspoon

[Download and docs](http://www.hammerspoon.org/).

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

## BetterSnapTool

Window movement and resizing via the keyboard.

## Vimium (Chrome Extension)

Vim bindings for navigating in Chrome.

