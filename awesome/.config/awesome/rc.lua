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

require("config")
