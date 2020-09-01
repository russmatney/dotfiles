local awful = require("awful")
local posix = require("posix")
require("./table-serialization")
require("./table-indexof")

-- `layouts` defined in run-init.fnl
--
-- This save/restore assumes the tags maintain their order.
-- It'd probably be better to match on tag names.

local tags_state_file = awful.util.get_cache_dir() .. "/tags-state"
local clients_state_file = awful.util.get_cache_dir() .. "/clients-state"

local mod = {}

function mod.save_state()
  -- clear and save tags
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


  -- clear and save clients
  local clients = client.get()

  local clients_to_restore = {}

  for i, c in ipairs(clients) do
    -- pp({tag=c:tags()[1].name,
    --     first_tag=c.first_tag.name,
    --     client=c.name})
    table.insert(clients_to_restore, {
      i,
      c.window,
      c.name,
      c.first_tag.name
    })
  end

  if posix.stat(clients_state_file) ~= nil then
    os.remove(clients_state_file)
  end

  table.save(clients_to_restore, clients_state_file)
end

function mod.save_state_and_restart()
  -- TODO syntax check/verify on config files before restarting
  mod.save_state()
  awesome.restart()
end

function mod.restore_state()
  if posix.stat(tags_state_file) ~= nil then
    local tags_to_restore = table.load(tags_state_file)

    local s = awful.screen.focused()
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
      else
        pp({missed_tag_cache_for=name})
      end
    end
  end

  if posix.stat(clients_state_file) ~= nil then
    local clients_to_restore = table.load(clients_state_file)

    for _, p in ipairs(clients_to_restore) do
      local window = p[2]
      local name = p[3]
      local tag = p[4]

      local c
      for _, cl in pairs(client.get()) do
        if cl.window == window then
          c = cl
        end
      end

      if c then
        local t = awful.tag.find_by_name(s, name)
        if t then
          c:tags({t})
        end
      else
        pp({missed_cached_client=name})
      end
    end
  end
end

return mod
