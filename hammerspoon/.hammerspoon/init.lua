
-- Function to toggle application visibility
function toggleApp(appName)
    local app = hs.application.get(appName)
    if app then
        if app:isFrontmost() then
            app:hide()
        else
            app:activate()
        end
    else
        hs.application.launchOrFocus(appName)
    end
end

-- Function to toggle 'floating-emacs'
-- function toggleEmacs(title)
--     local emacsApp = hs.application.get("Emacs")
--     if emacsApp then
--         local emacsWindows = emacsApp:allWindows()
--         local Focused = false

--         for _, window in ipairs(emacsWindows) do
--             if window:isVisible() and window:title() == title then
--                 if window:isFrontmost() then
--                     Focused = true
--                 end
--             end
--         end

--         if Focused then
--             -- If 'floating-emacs' is focused, hide it
--             for _, window in ipairs(emacsWindows) do
--                 if window:title() == title then
--                     window:hide()
--                     return
--                 end
--             end
--         else
--             -- If not focused, activate 'floating-emacs'
--             hs.application.launchOrFocus("Emacs")
--             for _, window in ipairs(emacsWindows) do
--                 if window:title() == title then
--                     window:focus()
--                     return
--                 end
--             end
--         end
--     else
--         hs.application.launchOrFocus("Emacs")
--     end
-- end


-- Hotkeys to toggle scratchpads
hs.hotkey.bind({"cmd"}, "t", function() toggleApp("Google Chrome") end)
hs.hotkey.bind({"cmd", "alt"}, "s", function() toggleApp("Spotify") end)
hs.hotkey.bind({"cmd", "alt"}, "v", function() toggleApp("Code") end)
hs.hotkey.bind({"cmd", "alt"}, "a", function() toggleApp("Slack") end)
hs.hotkey.bind({"cmd", "alt"}, "o", function() toggleApp("Obsidian") end)
hs.hotkey.bind({"cmd"}, "u", function() toggleApp("Obsidian") end)
hs.hotkey.bind({"cmd", "alt"}, "n", function() toggleApp("Neovide") end)
-- hs.hotkey.bind({"cmd", "alt"}, "d", function() toggleApp("Discord") end)
-- hs.hotkey.bind({"cmd"}, "e", function() toggleEmacs("floating-emacs") end)
-- hs.hotkey.bind({"cmd"}, "u", function() toggleEmacs("journal") end)
hs.hotkey.bind({"cmd"}, "e", function() toggleApp("Emacs") end)


-- Optional: Display a notification when toggling
hs.hotkey.bind({"cmd", "alt", "shift"}, "N", function()
    hs.notify.new({
        title = "Hammerspoon",
        informativeText = "Notif fired!"
    }):send()
end)
