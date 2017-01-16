# Dotfiles


# CLI

## Utils

### [`fzf`](https://github.com/junegunn/fzf)

### `j/autojump`

### `Ag/sift`

### `httpie`

### `tig`

## Zsh

### Aliases

# Tmux

## `tt` and tmux sessions

## Powerline

## Quirks

## `reattach-to-user-namespace`

```
brew install reattach-to-user-namespace
```

## `ctrl-H` movement fix

```
infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti ; tic $TERM.ti
```

# Vim

## Background and Resources

  - reading/writing a remap, ctrl/shift/etc

## Customizations

  - spacebar as leader
  - caps lock as escape (via OSX)
  - quick splits remaps

## Plugins

Use [vim-plug](https://github.com/junegunn/vim-plug).

  - `Airline`


# OSX

## Alfred

## Dash

## Hammerspoon

[Download and docs](http://www.hammerspoon.org/)

### Meta + key -> focus on any app

- `opt + t` -> focus on iTerm
- `opt + c` -> focus on Chrome

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
appShortcut(alt, 'F', 'Firefox')
```


## 'hacks'

## Other apps


# Dependencies

## Brew

## Node

## Python

## Elixir

## Golang
