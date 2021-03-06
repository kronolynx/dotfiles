-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")
ins = require("inspect")
-- Standard awesome library
local awful = require("awful")
require("awful.autofocus")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")

local gears = require("gears")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify(
    {
      preset = naughty.config.presets.critical,
      title = "Oops, there were errors during startup!",
      text = awesome.startup_errors
    }
  )
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal(
    "debug::error",
    function(err)
      -- Make sure we don't go into an endless error loop
      if in_error then
        return
      end
      in_error = true

      naughty.notify(
        {
          preset = naughty.config.presets.critical,
          title = "Oops, an error happened!",
          text = tostring(err)
        }
      )
      in_error = false
    end
  )
end
-- }}}

-- global variable definitions
require("variables")

-- Themes define colours, icons, font and wallpapers. (must be loaded before widgets)
beautiful.init(string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), theme))

-- ----------------------------------------------------------------------------------------
-- --    Start customization here

-- ===================================================================
-- Initialize icons array and load icon theme
local icons = require("icons")
icons.init(icon_theme)
-- -- imports
helpers = require("helpers")

-- >> - Desktop components
-- notification appearance
require("components.notifications")
-- Statusbar(s)
require("components.bar." .. bar_theme)
-- Exit screen
require("components.exit_screen." .. exit_screen_theme)
-- Window switcher
require("components.window_switcher")

-- -- notifications icon size
-- -- naughty.config.defaults['icon_size'] = 100 -- for older awesome versions
beautiful.notification_icon_size = 100 -- for awesome 4.3+

-- -- keys
keys = require("keys")

-- Signals
require("signals")

-- rules
require("rules")

-- autorun
awful.spawn.with_shell("~/.config/awesome/autorun.sh")

-- ===================================================================
-- Screen Change Functions (ie multi monitor)
-- ===================================================================


-- Reload config when screen geometry changes
screen.connect_signal("property::geometry", awesome.restart)

-- gears.timer.start_new(
--   10,
--   function()
--     collectgarbage("step", 20000)
--     return true
--   end
-- )

collectgarbage("setpause", 110)
collectgarbage("setstepmul", 1000)