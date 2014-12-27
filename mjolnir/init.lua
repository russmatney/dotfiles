local application = require "mjolnir.application"
local hotkey = require "mjolnir.hotkey"
local window = require "mjolnir.window"
local fnutils = require "mjolnir.fnutils"

local modalKey = {"alt"}

local resizeMappings = {
  h={x=0, y=0, w=0.5, h=1},
  j={x=0, y=0.5, w=1, h=0.5},
  k={x=0, y=0, w=1, h=0.5},
  l={x=0.5, y=0, w=0.5, h=1},
  f={x=0, y=0, w=1, h=1}
}

for key in pairs(resizeMappings) do
  hotkey.bind(modalKey, key, function()
    local win = window.focusedwindow()
    if win then win:movetounit(resizeMappings[key]) end
  end)
end

hotkey.bind(modalKey, "r", function()
  mjolnir.reload()
end)

local focusKeys = {
  c='Google Chrome',
  t='iTerm',
  a='Slack',
  p='Spotify',
  s='iOS Simulator',
  m='Messages',
  x='Xcode'
}
for key in pairs(focusKeys) do
  hotkey.bind(modalKey, key, function()
    application.launchorfocus(focusKeys[key])
  end)
end
