-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- this finds generated fennel.lua at /usr/lib/lua/<version>/fennel.lua
local fennel = require("fennel")
-- tells the path to support requiring `.fnl` files
fennel.path = fennel.path .. ";.config/awesome/?.fnl"
-- requires luarocks installed and available to root
-- (may need reboot after installing)
table.insert(package.loaders or package.searchers, fennel.searcher)

-- provides helpers for working with lua tables
require("fennelview")
-- require("./remote")

local cfg = require("cfg")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- Freedesktop menu
local freedesktop = require("freedesktop")
-- Enable VIM help for hotkeys widget when client with matching name is opened:
require("awful.hotkeys_popup.keys.vim")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                                  title = "Oops, there were errors during startup!",
                                  text = awesome.startup_errors })
end
-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true
        naughty.notify({ preset = naughty.config.presets.critical,
                                      title = "Oops, an error happened!",
                                      text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("/usr/share/awesome/themes/cesious/theme.lua")
beautiful.icon_theme        = "Papirus-Dark"
beautiful.bg_normal         = "#141A1B"
beautiful.bg_focus          = "#222B2E"
beautiful.font              = "Noto Sans Regular 10"
beautiful.notification_font = "Noto Sans Bold 14"
-- }}}

-- {{{ Signals

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Disable borders on lone windows
-- Handle border sizes of clients.
for s = 1, screen.count() do screen[s]:connect_signal("arrange", function ()
  local clients = awful.client.visible(s)
  local layout = awful.layout.getname(awful.layout.get(s))

  for _, c in pairs(clients) do
    -- No borders with only one humanly visible client
    if c.maximized then
      -- NOTE: also handled in focus, but that does not cover maximizing from a
      -- tiled state (when the client had focus).
      c.border_width = 0
    elseif c.floating or layout == "floating" then
      c.border_width = beautiful.border_width
    elseif layout == "max" or layout == "fullscreen" then
      c.border_width = 0
    else
      local tiled = awful.client.tiled(c.screen)
      if #tiled == 1 then -- and c == tiled[1] then
        tiled[1].border_width = 0
        -- if layout ~= "max" and layout ~= "fullscreen" then
        -- XXX: SLOW!
        -- awful.client.moveresize(0, 0, 2, 0, tiled[1])
        -- end
      else
        c.border_width = beautiful.border_width
      end
    end
  end
end)
end

-- }}}

awful.spawn.with_shell("~/.config/awesome/autorun.sh")


---------------------------------------------------------------------------
--- Remote control module allowing usage of awesome-client.
--
-- @author Julien Danjou &lt;julien@danjou.info&gt;, with tweaks from Rob Hoelz
-- @copyright 2009 Julien Danjou
-- @module awful.remote
---------------------------------------------------------------------------

-- Grab environment we need
require("awful.dbus")
local load = load
local tostring = tostring
local ipairs = ipairs
local table = table
local unpack = table.unpack
local dbus = dbus
local type = type

local client_env_mt = {}
local client_env = setmetatable({}, client_env_mt)

-- lazily set up awful (otherwise we get into infinite recursion madness)
function client_env_mt:__index(key)
    client_env.awful = require 'awful'

    local naughty = require 'naughty'

    function client_env.print(...)
        local parts = { ... }

        for i = 1, #parts do
            parts[i] = tostring(parts[i])
        end

        naughty.notify {
            title = 'awesome-client',
            text  = table.concat(parts)
        }
    end

    setmetatable(client_env, {__index = _G})

    -- we've overridden the metatable with a new one that delegates to _G, so
    -- use that logic to finish the job
    return client_env[key]
end

if dbus then
    dbus.connect_signal("org.awesomewm.awful.Remote", function(data, code)
        if data.member == "Eval" then
            local f, e = load(code, '=(load)', 't', client_env)
            if not f then
                return "s", e
            end
            local results = { pcall(f) }
            if not table.remove(results, 1) then
                return "s", "Error during execution: " .. tostring(results[1])
            end
            local retvals = {}
            for _, v in ipairs(results) do
                local t = type(v)
                if t == "boolean" then
                    table.insert(retvals, "b")
                    table.insert(retvals, v)
                elseif t == "number" then
                    table.insert(retvals, "d")
                    table.insert(retvals, v)
                else
                    table.insert(retvals, "s")
                    table.insert(retvals, tostring(v))
                end
            end
            return unpack(retvals)
        end
    end)
end

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
