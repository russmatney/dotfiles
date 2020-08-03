-------------------------------------------------
-- Originally gutted from:
-- https://github.com/streetturtle/awesome-wm-widgets/tree/master/todo-widget
-------------------------------------------------

-- TODO add click to refresh

local wibox = require("wibox")
local json = require("rxi-json-lua")
local spawn = require("awful.spawn")

local HOME_DIR = os.getenv("HOME")
local FOCUS_FILE = HOME_DIR .. '/todo/focus.json'
local GET_FOCUS = 'bash -c "cat ' .. FOCUS_FILE .. '"'
local UPDATE_FOCUS = 'bash -c "ralphie focus update-json"'

local focus_widget = {}

focus_widget.widget = wibox.widget {
  { markup =
      '<span size="large" font_weight="bold" color="#efaefb">Focus</span>',
    align = 'center',
    forced_width = 350, -- for horizontal alignment
    forced_height = 40,
    widget = wibox.widget.textbox
  },
  { id = "txt",
    widget = wibox.widget.textbox
  },
  layout = wibox.layout.fixed.horizontal,
  set_text = function(self, new_value)
      self.txt.text = new_value
  end,
}

function focus_widget:update_focus(latest_focus)
  local str = latest_focus.name
    -- .. "#" .. latest_focus.tags
  focus_widget.widget:set_text(str);
end

local function worker(args)
    local args = args or {}

    function update_focus_widget(stdout)
        local result = json.decode(stdout)

        if result == nil or result == '' then
          result = {}
        end

        focus_widget:update_focus(result.latest_focus)
    end

    spawn.easy_async(UPDATE_FOCUS, function()
                       spawn.easy_async(GET_FOCUS, function(stdout)
                                          update_focus_widget(stdout)
                       end)
    end)

    return focus_widget.widget
end

return setmetatable(focus_widget, { __call = function(_, ...) return worker(...) end })
