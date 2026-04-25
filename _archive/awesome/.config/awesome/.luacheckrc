-- this luacheckrc originally pulled from awesome repo
-- edited with pase of ignores and globals from working on chess-em-up

-- Only allow symbols available in all Lua versions
std = "min"

-- Get rid of "unused argument self"-warnings
self = false

-- Theme files, ignore max line length
ignore = {
  "21/_.*",
  "431",
  "111", -- allow globals in fennel :yeehaw:
  "631", -- line too long. mostly not in our control
  "541", -- empty do-end blocks? seem to show up (: some :times)
  "311" -- unused var (false positive with let/when/-?>)
}

-- Global objects defined by the C code
read_globals = {
    "awesome",
    "button",
    "dbus",
    "drawable",
    "drawin",
    "key",
    "keygrabber",
    "mousegrabber",
    "selection",
    "tag",
    "window",
    "table.unpack",
    "math.atan2",
    "package",
}

new_globals = {
  "love", "pp", "fennel", "lume", "u", "ppi",
  "layouts",
}

-- screen may not be read-only, because newer luacheck versions complain about
-- screen[1].tags[1].selected = true.
-- The same happens with the following code:
--   local tags = mouse.screen.tags
--   tags[7].index = 4
-- client may not be read-only due to client.focus.
globals = {
    "screen",
    "mouse",
    "root",
    "client"
}

-- Enable cache (uses .luacheckcache relative to this rc file).
cache = true

stds.fennel = {globals ={"fennel", "pp", "lume", "self", "u"}}
