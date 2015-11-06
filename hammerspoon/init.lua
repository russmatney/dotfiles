-- general

function fancyNotify(message)
     hs.notify.new({title="Hammerspoon", informativeText=message}):send():release()
end



-- app shortcuts

function appShortcut(modifier, character, application)
  hs.hotkey.bind(modifier, character, function() hs.application.launchOrFocus(application) end)
end

local alt = {'alt'}

appShortcut(alt, 'C', 'Google Chrome')
appShortcut(alt, 'T', 'iTerm')



-- reload config

function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end

hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
fancyNotify("Config Loaded")
