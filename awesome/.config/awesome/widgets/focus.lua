-------------------------------------------------
-- Originally gutted from:
-- https://github.com/streetturtle/awesome-wm-widgets/tree/master/todo-widget
-------------------------------------------------

local wibox = require("wibox")
local json = require("rxi-json-lua")
local spawn = require("awful.spawn")

local HOME_DIR = os.getenv("HOME")
local UPDATE_FOCUS = 'bash -c "ralphie set-focus first"'

local focus_widget = {}

focus_widget.widget = wibox.widget {
  { markup =
      '<span size="large" font_weight="bold" color="#536452">Current Focus: </span>',
    align = 'center',
    -- forced_width = 350, -- for horizontal alignment
    -- forced_height = 40,
    widget = wibox.widget.textbox
  },
  { id = "txt",
    widget = wibox.widget.textbox
  },
  layout = wibox.layout.fixed.horizontal,
  set_text = function(self, new_value)
    local str = '<span size="large" font_weight="bold" color="#efaefb">' ..
      new_value .. '</span>';
    self.txt.markup = str
  end,
}

function focus_widget:update_focus(latest_focus)
  focus_widget.widget:set_text(latest_focus.name);
end

local function worker(args)
    local args = args or {}

    -- global function called by ralphie via dbus repl
    function update_focus_widget(focus)
        pp(focus)
        focus_widget:update_focus(focus)
    end

    -- depends on update callback from ralphie
    spawn.easy_async(UPDATE_FOCUS, function () print "focus update requested" end);

    return focus_widget.widget
end

return setmetatable(focus_widget, { __call = function(_, ...) return worker(...) end })
