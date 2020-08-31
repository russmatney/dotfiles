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

  local tags_to_restore = {}

  for i, t in ipairs(tags) do
    local sel
    if t.selected == true then
      sel = 'true'
    else
      sel = 'false'
    end

    table.insert(tags_to_restore, {
      i,
      t.name,
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

  table.save(tags_to_restore, tags_state_file)
end

function obj.save_state_and_restart()
  -- TODO syntax check/verify on config files before restarting
  obj.save_state()
  awesome.restart()
end

function obj.restore_state()
  if posix.stat(tags_state_file) ~= nil then
    local tags_to_restore = table.load(tags_state_file)

    -- TODO handle creating tags here if they aren't found
    -- TODO refactor to find a tag for the name rather than use the index
    local s = awful.screen.focused()
    for _j, p in ipairs(s.tags) do
      pp(p)
    end
    for j, p in ipairs(tags_to_restore) do
      local i = p[1]
      local name = p[2]
      local layout = p[3] -- index of layout in layouts table
      local ncol = p[4]
      local mwfact = p[5]
      local nmaster = p[6]
      local selected = p[7] == 'true'

      local t = awful.tag.find_by_name(s, name)

      if not t then
        pp({creating_cached_tag=name})
        awful.tag.add(name, {screen=s})
        t = awful.tag.find_by_name(s, name)
      end

      if t then
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
end

return obj
