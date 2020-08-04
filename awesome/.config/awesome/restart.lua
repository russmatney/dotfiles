local awful = require("awful")
local posix = require("posix")
require("./table-serialization")
require("./table-indexof")

-- `layouts` defined in run-init.fnl
--
-- This save/restore assumes the tags maintain their order.
-- It'd probably be better to match on tag names.

local tags_state_file = awful.util.get_cache_dir() .. "/state"

local obj = {}

function obj.save_state()
  local screen = mouse.screen
  local tags = screen.tags

  local params = {}

  for i, t in ipairs(tags) do
    local sel
    if t.selected == true then
      sel = 'true'
    else
      sel = 'false'
    end

    table.insert(params, {
      i,
      table.indexof(layouts, t.layout),
      t.column_count,
      t.master_width_factor,
      t.master_count,
      sel,
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
      local selected = p[6] == 'true'

      local t = s.tags[i]
      t.layout = layouts[layout]

      t.column_count = ncol
      t.master_width_factor = mwfact
      t.master_count = nmaster

      if selected and t.selected == false then
        awful.tag.viewtoggle(t);
      end
    end
  end
end

return obj
