local wibox = require("wibox")
local spawn = require("awful.spawn")
local tablex = require("pl.tablex")

local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")

local UPDATE_REPOS = 'bash -c "ralphie update-dirty-repos"'

local function row (item)
  return wibox.widget {
    {
      {
        text = item.name,
        align = 'left',
        widget = wibox.widget.textbox
      },
      margins = 8,
      layout = wibox.container.margin
    },
    bg = beautiful.bg_normal,
    widget = wibox.container.background,
  }
end

local popup = awful.popup{
  bg = beautiful.bg_normal,
  ontop = true,
  visible = false,
  shape = gears.shape.rounded_rect,
  border_width = 1,
  border_color = beautiful.bg_focus,
  maximum_width = 400,
  offset = { y = 5 },
  widget = {}
}

local repos_widget = {}

repos_widget.widget = wibox.widget {
  { markup =
      '<span size="large" font_weight="bold" color="#536452">' ..
      'Dirty Repos: </span>',
    align = 'center',
    widget = wibox.widget.textbox
  },
  { id = "txt",
    widget = wibox.widget.textbox
  },
  layout = wibox.layout.fixed.horizontal,
  set_count = function(self, new_value)
    local str = '<span size="large" font_weight="bold" color="#efaefb">' ..
      new_value .. '</span>';
    self.txt.markup = str
  end,
}

function repos_widget:update_count(dirty_repos)
  if dirty_repos then
    local count = tablex.size(dirty_repos);
    repos_widget.widget:set_count(count);
  end
end

local function worker()
  -- global function called by ralphie via dbus repl
  function update_repos_widget(repos)
    repos_widget:update_count(repos)

    local rows = {layout = wibox.layout.fixed.vertical}

    for _, item in ipairs(repos) do
      table.insert(rows, row(item))
    end

    popup:setup(rows)

  end

  repos_widget.widget:connect_signal(
    "mouse::enter",
    function()
      if not popup.visible then
        popup.visible = true
        popup:move_next_to(mouse.current_widget_geometry)
      end
    end)
  repos_widget.widget:connect_signal(
    "mouse::leave",
    function()
      if popup.visible then
        popup.visible = false
      end
    end)

    repos_widget.widget:buttons(
            awful.util.table.join(
                    awful.button({}, 1, function()
                        spawn.easy_async(UPDATE_REPOS,
                                         function () print "repos update requested" end);
                    end)))

  -- depends on update callback from ralphie
  spawn.easy_async(UPDATE_REPOS,
                   function () print "repos update requested" end);

  return repos_widget.widget
end

return setmetatable(repos_widget, { __call = function(_, ...) return worker(...) end })
