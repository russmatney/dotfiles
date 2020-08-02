print("\n\n\nWelcome to a new awesome lifespan!\n\n\n")

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

package.path = (package.path -- luacheck:ignore 122
                  .. ";/home/russ/streetturtle/?.lua")

-- this finds generated fennel.lua at /usr/lib/lua/<version>/fennel.lua
local fennel = require("fennel")
-- tells the path to support requiring `.fnl` files in the awesome dir
fennel.path = fennel.path .. ";.config/awesome/?.fnl"

-- requires luarocks installed and available to root
-- (may need reboot after installing)
table.insert(package.loaders or package.searchers, fennel.searcher)

-- TODO move these into the theme
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()
-- Make dpi function global
dpi = beautiful.xresources.apply_dpi
-- Make xresources colors global
x = {
    --           xrdb variable
    background = xrdb.background,
    foreground = xrdb.foreground,
    color0     = xrdb.color0,
    color1     = xrdb.color1,
    color2     = xrdb.color2,
    color3     = xrdb.color3,
    color4     = xrdb.color4,
    color5     = xrdb.color5,
    color6     = xrdb.color6,
    color7     = xrdb.color7,
    color8     = xrdb.color8,
    color9     = xrdb.color9,
    color10    = xrdb.color10,
    color11    = xrdb.color11,
    color12    = xrdb.color12,
    color13    = xrdb.color13,
    color14    = xrdb.color14,
    color15    = xrdb.color15,
}

-- Setup done, start 'er up
require("run-init")
