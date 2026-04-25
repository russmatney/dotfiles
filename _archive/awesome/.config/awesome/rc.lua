print("\n\n\nWelcome to a new awesome lifespan!\n")

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

package.path = (package.path -- luacheck:ignore 122
                .. ";/home/russ/streetturtle/?.lua"
                  .. ";/home/russ/.luaver/luarocks/3.3.1_5.3/share/lua/5.3/?.lua"
                  .. ";/home/russ/.luaver/luarocks/3.3.1_5.3/share/lua/5.3/?/init.lua"
               )
package.cpath = (package.cpath -- luacheck:ignore 122
                  .. ";/home/russ/.luaver/luarocks/3.3.1_5.3/lib/lua/5.3/?.so")

-- this finds generated fennel.lua at /usr/lib/lua/<version>/fennel.lua
local fennel = require("fennel")
-- tells the path to support requiring `.fnl` modules in the awesome dir
fennel.path = fennel.path .. ";.config/awesome/?.fnl"

-- requires luarocks installed and available to root
-- LUA_PATH set in .zshenv
-- (may need reboot after installing)
table.insert(package.loaders or package.searchers, fennel.searcher)

-- Setup done, start 'er up
require("run-init")
