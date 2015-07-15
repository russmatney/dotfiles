local application = require "mjolnir.application"
local hotkey = require "mjolnir.hotkey"
local window = require "mjolnir.window"
local fnutils = require "mjolnir.fnutils"

local modalKey = {"alt"}

local resizeMappings = {
  f={x=0, y=0, w=1, h=1}
}

for key in pairs(resizeMappings) do
  hotkey.bind(modalKey, key, function()
    local win = window.focusedwindow()
    if win then win:movetounit(resizeMappings[key]) end
  end)
end

local focusKeys = {
  c='Google Chrome',
  d='Dash',
  e='System Preferences',
  p='Spotify',
  i='iTunes',
  t='iTerm',
  r='LimeChat',
  m='Messages',
  n='nvALT'
}
for key in pairs(focusKeys) do
  hotkey.bind(modalKey, key, function()
    application.launchorfocus(focusKeys[key])
  end)
end
