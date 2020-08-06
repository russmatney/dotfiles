local gears = require("gears")
local wibox = require("wibox")
local spawn = require("awful.spawn")

local UPDATE_POMODORO = 'bash -c "ralphie update-pomodoro-widget"'

local pomodoro_widget = {}

pomodoro_widget.widget = wibox.widget {
    { markup =
          '<span size="large" font_weight="bold" color="#536452">Pomo time: </span>',
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

function pomodoro_widget:update_pomodoro(pomodoro_str)
    pomodoro_widget.widget:set_text(pomodoro_str);
end

local function worker()
    -- global function called by ralphie via dbus repl
    function update_pomodoro_widget(pomodoro_str)
        pomodoro_widget:update_pomodoro(pomodoro_str)
    end

    gears.timer {
        timeout   = 60,
        call_now  = true,
        autostart = true,
        callback  = function()
          spawn.easy_async(UPDATE_POMODORO, function() print "Pomodoro update requested" end)
        end
    }

    return pomodoro_widget.widget
end

return setmetatable(pomodoro_widget, { __call = function(_, ...) return worker(...) end })
