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

-- Setup done, start 'er up
require("run-init")
