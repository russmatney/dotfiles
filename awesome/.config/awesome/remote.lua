-- repl on dbus helper found here:
-- https://gist.github.com/hoelzro/872af8bc9b27bb348aba5f93ce3e8158
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

-- lazily set up awful (otherwise we get circular deps)
function client_env_mt:__index(key)
    client_env.awful = require 'awful'

    local naughty = require 'naughty'

    function client_env.print(...)
        print "overwritten print?"
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

function init_remote()
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
end
