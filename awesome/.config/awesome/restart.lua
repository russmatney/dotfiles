local awful = require("awful")
local posix = require("posix")
require("./table-serialization")
require("./table-indexof")

local tags_state_file = awful.util.get_cache_dir() .. "/state"

local obj = {}

function obj.save_state()
  local screen = mouse.screen
  local tags = awful.tag.gettags(screen)

  local params = {}

  for i, t in ipairs(tags) do
    print "saving tag"
    print(t.name)
    print(t.selected)
    table.insert(params, {
      i,
      table.indexof(layouts, t.layout),
      awful.tag.getncol(t),
      awful.tag.getmwfact(t),
      awful.tag.getnmaster(t),
      t.selected,
    })
  end

  if posix.stat(tags_state_file) ~= nil then
    os.remove(tags_state_file)
  end

  table.save(params, tags_state_file)
end

function obj.save_state_and_restart()
  -- TODO syntax check/verify on config files before restarting
  obj.save_state()
  awesome.restart()
end

function obj.restore_state()
  if posix.stat(tags_state_file) ~= nil then
    local params = table.load(tags_state_file)

    local s = awful.screen.focused()
    for j, p in ipairs(params) do
      local i = p[1]
      local layout = p[2] -- index of layout in layouts table
      local ncol = p[3]
      local mwfact = p[4]
      local nmaster = p[5]
      local selected = p[6]

      local t = s.tags[i]
      t.layout = layouts[layout]

      awful.tag.setncol(ncol, t)
      awful.tag.setmwfact(mwfact, t)
      awful.tag.setnmaster(nmaster, t)

      print "restoring tag"
      print(t.name)
      print(t.selected)
      print(selected)

      if selected and t.selected == false then
        awful.tag.viewtoggle(t);
      end

      print(t.selected)
    end
  end
end

return obj
