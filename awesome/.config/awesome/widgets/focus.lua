local awful = require("awful")
local wibox = require("wibox")
local spawn = require("awful.spawn")

local UPDATE_FOCUS_LATEST = 'bash -c "ralphie set-focus first"'
local UPDATE_FOCUS_ROFI = 'bash -c "ralphie set-focus"'

local focus_widget = {}

focus_widget.widget = wibox.widget {
    { markup =
          '<span size="large" font_weight="bold" color="#536452">Focus: </span>',
      align = 'center',
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

local function worker()
    -- global function called by ralphie via dbus repl
    function update_focus_widget(focus)
        focus_widget:update_focus(focus)
    end

    focus_widget.widget:buttons(
            awful.util.table.join(
                    awful.button({}, 1, function()
                        spawn.easy_async(UPDATE_FOCUS_ROFI,
                                         function () print "focus via rofi requested" end);
                    end)))

    -- depends on update callback from ralphie
    spawn.easy_async(UPDATE_FOCUS_LATEST, function () print "latest focus requested" end);

    return focus_widget.widget
end

return setmetatable(focus_widget, { __call = function(_, ...) return worker(...) end })
