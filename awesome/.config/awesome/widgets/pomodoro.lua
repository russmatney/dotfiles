local gears = require("gears")
local wibox = require("wibox")
local spawn = require("awful.spawn")

local UPDATE_POMODORO = 'bash -c "ralphie update-pomodoro-widget"'

local pomodoro_widget = {}

pomodoro_widget.widget = wibox.widget {
    {
      id = "lbl",
      widget = wibox.widget.textbox
    },
    { id = "txt",
      widget = wibox.widget.textbox
    },
    layout = wibox.layout.fixed.horizontal,
    set_text = function(self, new_label, new_value)
        -- print(new_value)
        local str = '<span size="large" font_weight="bold" color="#efaefb">' ..
            new_value .. '</span>';
        self.txt.markup = str

        -- print(new_label)
        local label_str = '<span size="large" font_weight="bold" color="#536452">' ..
            new_label .. '</span>';
        self.lbl.markup = label_str
    end,
}

function pomodoro_widget:update_pomodoro(msg)
    pomodoro_widget.widget:set_text(msg.label, msg.value);
end

local function worker()
    -- global function called by ralphie via dbus repl
    function update_pomodoro_widget(msg)
        pomodoro_widget:update_pomodoro(msg)
    end

    gears.timer {
        timeout   = 60,
        call_now  = true,
        autostart = true,
        callback  = function()
            spawn.easy_async(UPDATE_POMODORO, function()
                                 -- print "Pomodoro update requested"
            end)
        end
    }

    return pomodoro_widget.widget
end

return setmetatable(pomodoro_widget, { __call = function(_, ...) return worker(...) end })
