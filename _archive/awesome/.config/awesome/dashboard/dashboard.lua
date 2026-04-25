-- pulled and gutted from https://github.com/elenapan/dotfiles/blob/master/config/awesome/elemental/dashboard/amarena.lua

local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")

local helpers = require("./dashboard/helpers")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local keygrabber = require("awful.keygrabber")

-- load settings
-- local xrdb = beautiful.xresources.get_current_theme()
-- Make dpi function global
dpi = beautiful.xresources.apply_dpi
-- Make xresources colors global
x = {
    --           xrdb variable
    background = "#000000",
    foreground = "#000000",
    color0     = "#000000",
    color1     = "#000000",
    color2     = "#000000",
    color3     = "#000000",
    color4     = "#000000",
    color5     = "#000000",
    color6     = "#000000",
    color7     = "#000000",
    color8     = "#000000",
    color9     = "#000000",
    color10    = "#000000",
    color11    = "#000000",
    color12    = "#000000",
    color13    = "#000000",
    color14    = "#000000",
    color15    = "#000000",
}

-- Appearance
local box_radius = beautiful.dashboard_box_border_radius or dpi(12)
local box_gap = dpi(6)

-- Get screen geometry
local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

-- Create the widget
dashboard = wibox({visible = false, ontop = true, type = "dock", screen = screen.primary})
awful.placement.maximize(dashboard)

dashboard.bg = beautiful.dashboard_bg or beautiful.exit_screen_bg or beautiful.wibar_bg or "#111111"
dashboard.fg = beautiful.dashboard_fg or beautiful.exit_screen_fg or beautiful.wibar_fg or "#FEFEFE"

-- Add dashboard or mask to each screen
for s in screen do
    if s == screen.primary then
        s.dashboard = dashboard
    else
        s.dashboard = helpers.screen_mask(s, dashboard.bg)
    end
end

local function set_visibility(v)
    for s in screen do
        s.dashboard.visible = v
    end
end

dashboard:buttons(gears.table.join(
    -- Middle click - Hide dashboard
    awful.button({ }, 2, function ()
        dashboard_hide()
    end)
))


local dashboard_grabber
function dashboard_hide()
    awful.keygrabber.stop(dashboard_grabber)
    set_visibility(false)
end


local original_cursor = "left_ptr"
function dashboard_show()
    -- Fix cursor sometimes turning into "hand1" right after showing the dashboard
    -- Sigh... This fix does not always work
    local w = mouse.current_wibox
    if w then
        w.cursor = original_cursor
    end
    -- naughty.notify({text = "starting the keygrabber"})
    dashboard_grabber = awful.keygrabber.run(function(_, key, event)
        if event == "release" then return end
        -- Press Escape or q or F1 to hide it
        if key == 'Escape' or key == 'q' or key == 'F1' then
            dashboard_hide()
        end
    end)
    set_visibility(true)
end


-- Helper function that puts a widget inside a box with a specified background color
-- Invisible margins are added so that the boxes created with this function are evenly separated
-- The widget_to_be_boxed is vertically and horizontally centered inside the box
local function create_boxed_widget(widget_to_be_boxed, width, height, bg_color)
    local box_container = wibox.container.background()
    box_container.bg = bg_color
    box_container.forced_height = height
    box_container.forced_width = width
    box_container.shape = helpers.rrect(box_radius)
    -- box_container.shape = helpers.prrect(20, true, true, true, true)
    -- box_container.shape = helpers.prrect(30, true, true, false, true)

    local boxed_widget = wibox.widget {
        -- Add margins
        {
            -- Add background color
            {
                -- Center widget_to_be_boxed horizontally
                nil,
                {
                    -- Center widget_to_be_boxed vertically
                    nil,
                    -- The actual widget goes here
                    widget_to_be_boxed,
                    layout = wibox.layout.align.vertical,
                    expand = "none"
                },
                layout = wibox.layout.align.horizontal,
                expand = "none"
            },
            widget = box_container,
        },
        margins = box_gap,
        color = "#FF000000",
        widget = wibox.container.margin
    }

    return boxed_widget
end



local username = os.getenv("USER")
local user_text = wibox.widget.textbox(username:upper())
-- user_text.font = "San Francisco Display Heavy 20"
user_text.align = "center"
user_text.valign = "center"

local host_text = wibox.widget.textbox()
awful.spawn.easy_async_with_shell("hostname", function(out)
    -- Remove trailing whitespaces
    out = out:gsub('^%s*(.-)%s*$', '%1')
    host_text.markup = helpers.colorize_text("@"..out, x.color8)
end)
-- host_text.markup = "<span foreground='" .. x.color8 .."'>" .. minutes.text .. "</span>"
-- host_text.font = "monospace 16"
host_text.align = "center"
host_text.valign = "center"
local user_widget = wibox.widget {
    user_text,
    helpers.vertical_pad(dpi(4)),
    host_text,
    layout = wibox.layout.fixed.vertical
}
local user_box = create_boxed_widget(user_widget, dpi(300), dpi(340), x.background)

local disk_arc = wibox.widget {
    start_angle = 3 * math.pi / 2,
    min_value = 0,
    max_value = 100,
    value = 50,
    border_width = 0,
    thickness = dpi(8),
    forced_width = dpi(90),
    forced_height = dpi(90),
    rounded_edge = true,
    bg = x.color8.."55",
    colors = { x.color13 },
    widget = wibox.container.arcchart
}

local disk_hover_text_value = wibox.widget {
    align = "center",
    valign = "center",
    -- font = "sans medium 13",
    widget = wibox.widget.textbox()
}
local disk_hover_text = wibox.widget {
    disk_hover_text_value,
    {
        align = "center",
        valign = "center",
        -- font = "sans medium 10",
        widget = wibox.widget.textbox("free")
    },
    spacing = dpi(2),
    visible = false,
    layout = wibox.layout.fixed.vertical
}

awesome.connect_signal("evil::disk", function(used, total)
    disk_arc.value = used * 100 / total
    disk_hover_text_value.markup = helpers.colorize_text(tostring(total - used).."G", x.color4)
end)

local disk = wibox.widget {
    {
        nil,
        disk_hover_text,
        expand = "none",
        layout = wibox.layout.align.vertical
    },
    disk_arc,
    top_only = false,
    layout = wibox.layout.stack
}

local disk_box = create_boxed_widget(disk, dpi(150), dpi(150), x.background)

disk_box:connect_signal("mouse::enter", function ()
    disk_icon.visible = false
    disk_hover_text.visible = true
end)
disk_box:connect_signal("mouse::leave", function ()
    disk_icon.visible = true
    disk_hover_text.visible = false
end)


-- File system bookmarks
local function create_bookmark(name, path, color, hover_color)
    local bookmark = wibox.widget.textbox()
    -- bookmark.font = "sans bold 16"
    -- bookmark.text = wibox.widget.textbox(name:sub(1,1):upper()..name:sub(2))
    bookmark.markup = helpers.colorize_text(name, color)
    bookmark.align = "center"
    bookmark.valign = "center"

    -- Buttons
    bookmark:buttons(gears.table.join(
        awful.button({ }, 1, function ()
            awful.spawn.with_shell(user.file_manager.." "..path)
            dashboard_hide()
        end),
        awful.button({ }, 3, function ()
            awful.spawn.with_shell(user.terminal.." -e 'ranger' "..path)
            dashboard_hide()
        end)
    ))

    -- Hover effect
    bookmark:connect_signal("mouse::enter", function ()
        bookmark.markup = helpers.colorize_text(name, hover_color)
    end)
    bookmark:connect_signal("mouse::leave", function ()
        bookmark.markup = helpers.colorize_text(name, color)
    end)

    helpers.add_hover_cursor(bookmark, "hand1")

    return bookmark
end

local bookmarks = wibox.widget {
    create_bookmark("home", os.getenv("HOME"), x.color1, x.color9),
    spacing = dpi(10),
    layout = wibox.layout.fixed.vertical
}

local bookmarks_box = create_boxed_widget(bookmarks, dpi(200), dpi(300), x.background)


-- Fortune
local fortune_command = "fortune -n 140 -s"
local fortune_update_interval = 3600
-- local fortune_command = "fortune -n 140 -s computers"
local fortune = wibox.widget {
    -- font = "sans medium 11",
    text = "Loading your cookie...",
    widget = wibox.widget.textbox
}

local update_fortune = function()
    awful.spawn.easy_async_with_shell(fortune_command, function(out)
        -- Remove trailing whitespaces
        out = out:gsub('^%s*(.-)%s*$', '%1')
        fortune.markup = "<i>"..helpers.colorize_text(out, x.color4).."</i>"
    end)
end

gears.timer {
    autostart = true,
    timeout = fortune_update_interval,
    single_shot = false,
    call_now = true,
    callback = update_fortune
}

local fortune_widget = wibox.widget {
    {
        nil,
        fortune,
        layout = wibox.layout.align.horizontal,
    },
    margins = box_gap * 4,
    color = "#00000000",
    widget = wibox.container.margin
}

local fortune_box = create_boxed_widget(fortune_widget, dpi(300), dpi(140), x.background)
fortune_box:buttons(gears.table.join(
    -- Left click - New fortune
    awful.button({ }, 1, update_fortune)
))
helpers.add_hover_cursor(fortune_box, "hand1")


-- Uptime
local uptime_text = wibox.widget.textbox()
awful.widget.watch("uptime -p | sed 's/^...//'", 60, function(_, stdout)
    -- Remove trailing whitespaces
    local out = stdout:gsub('^%s*(.-)%s*$', '%1')
    uptime_text.text = out
end)
local uptime = wibox.widget {
    {
        align = "center",
        valign = "center",
        -- font = "icomoon 20",
        markup = helpers.colorize_text("", x.color3),
        widget = wibox.widget.textbox()
    },
    {
        align = "center",
        valign = "center",
        -- font = "sans medium 11",
        widget = uptime_text
    },
    spacing = dpi(10),
    layout = wibox.layout.fixed.horizontal
}

local uptime_box = create_boxed_widget(uptime, dpi(300), dpi(80), x.background)

uptime_box:buttons(gears.table.join(
    awful.button({ }, 1, function ()
        exit_screen_show()
        gears.timer.delayed_call(function()
            dashboard_hide()
        end)
    end)
))
helpers.add_hover_cursor(uptime_box, "hand1")


-- Item placement
dashboard:setup {
    -- Center boxes vertically
    nil,
    {
        -- Center boxes horizontally
        nil,
        {
            -- Column container
            {
                -- Column 1
                user_box,
                fortune_box,
                layout = wibox.layout.fixed.vertical
            },
            {
                -- Column 2
                disk_box,
                layout = wibox.layout.fixed.vertical
            },
            {
                -- Column 3
                bookmarks_box,
                layout = wibox.layout.fixed.vertical
            },
            {
                -- Column 4
                uptime_box,
                layout = wibox.layout.fixed.vertical
            },
            layout = wibox.layout.fixed.horizontal
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.horizontal

    },
    nil,
    expand = "none",
    layout = wibox.layout.align.vertical
}


return {dashboard_show = dashboard_show}
