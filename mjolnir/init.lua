local application = require "mjolnir.application"
local hotkey = require "mjolnir.hotkey"
local window = require "mjolnir.window"
local fnutils = require "mjolnir.fnutils"

hotkey.bind({"cmd", "alt", "ctrl"}, "D", function()
  local win = window.focusedwindow()
  local f = win:frame()
  f.x = f.x + 10
  win:setframe(f)
end)

