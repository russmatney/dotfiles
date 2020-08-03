local awful = require("awful")
local posix = require("posix")
require("./table-serialization")
require("./table-indexof")

local my_state_file = awful.util.get_cache_dir() .. "/state"

local obj = {}

function obj.save_state()
  local screen = mouse.screen
  local tags = awful.tag.gettags(screen)

  local params = {}

  for i, t in ipairs(tags) do
    table.insert(params, {
      i,
      table.indexof(layouts, t.layout),
      awful.tag.getncol(t),
      awful.tag.getmwfact(t),
      awful.tag.getnmaster(t)
    })
  end

  if posix.stat(my_state_file) ~= nil then
    os.remove(my_state_file)
  end

  table.save(params, my_state_file)
end

function obj.save_state_and_restart()
  -- TODO syntax check/verify on config files before restarting
  obj.save_state()
  awesome.restart()
end

function obj.restore_state()
  if posix.stat(my_state_file) ~= nil then
    local params = table.load(my_state_file)

    local s = awful.screen.focused()
    for j, p in ipairs(params) do
      local i = p[1]
      local layout = p[2] -- index of layout in layouts table
      local ncol = p[3]
      local mwfact = p[4]
      local nmaster = p[5]

      local t = s.tags[i]
      t.layout = layouts[layout]

      awful.tag.setncol(ncol, t)
      awful.tag.setmwfact(mwfact, t)
      awful.tag.setnmaster(nmaster, t)
    end
  end
end

return obj
