local beautiful       = require("beautiful")
local gears           = require("gears")
local awful           = require("awful")

local helpers         = {}
-- Turn border color to back if there is only one client in the tag.
helpers.border_adjust = function(c)
  if #awful.screen.focused().clients == 1 then
    c.border_color = beautiful.border_normal
  else
    c.border_color = beautiful.border_focus
  end
end

helpers.set_wallpaper = function(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

return helpers
