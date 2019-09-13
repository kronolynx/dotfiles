-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox         = require("wibox")
-- Theme handling library
local beautiful     = require("beautiful")
-- Notification library
local naughty       = require("naughty")
local menubar       = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

local freedesktop   = require("freedesktop")

local utils         = require("utils")
local keys          = require("keys")
local rules         = require("rules")
local variables     = require("variables")


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({ preset = naughty.config.presets.critical,
                   title  = "Oops, there were errors during startup!",
                   text   = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function(err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify({ preset = naughty.config.presets.critical,
                     title  = "Oops, an error happened!",
                     text   = tostring(err) })
    in_error = false
  end)
end
-- }}}

-- Themes define colours, icons, font and wallpapers.
beautiful.init(string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), chosen_theme))
-- import widgets (requires theme to be loaded)
local widgets       = require("widgets")
-- notifications icon size
-- naughty.config.defaults['icon_size'] = 100 -- for older awesome versions
beautiful.notification_icon_size = 100 -- for awesome 4.3+


awful.util.mymainmenu = freedesktop.menu.build({
  icon_size = beautiful.menu_height or 16,
  before    = {
    { "Awesome", myawesomemenu, beautiful.awesome_icon },
    -- other triads can be put here
  },
  after     = {
    { "Open terminal", terminal },
    -- other triads can be put here
  }
})

local function set_wallpaper(s)
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

for s = 1, screen.count() do 
  set_wallpaper(s)
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- {{{ Screen

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s)
  -- Tags
  for _, i in pairs(awful.util.tagnames[s.index]) do
    awful.tag.add(i.name, {
      layout              = i.lay or awful.layout.layouts[1],
      gap                 = i.gap or beautiful.useless_gap,
      gap_single_client   = true,
      screen              = s,
      selected            = i.sel or false,
      master_width_factor = i.mw or 0.5,
    })
  end

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()
  -- Create an imagebox widget which will contains an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(gears.table.join(
      awful.button({}, 2, function() awful.layout.set(awful.layout.layouts[1]) end),
      awful.button({}, 4, function() awful.layout.inc(1) end),
      awful.button({}, 5, function() awful.layout.inc(-1) end)))

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
      screen  = s,
      filter  = awful.widget.taglist.filter.all,
      buttons = awful.util.taglist_buttons
  }
  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
      screen  = s,
      filter  = awful.widget.tasklist.filter.currenttags,
      buttons = tasklist_buttons
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
      screen  = s,
      filter  = awful.widget.tasklist.filter.currenttags,
      buttons = tasklist_buttons
  }

  -- Create the wibox
  s.mywibox    = awful.wibar({ position = "top", screen = s, height = 26 })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    expand = 'none',
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      s.mytaglist,
      s.mypromptbox,
      -- s.mytasklist,
      widgets.seperator,
    },
    nil,
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      id     = "rightwidgets",
      widgets.kblayout,
      widgets.vol,
      widgets.textclock,
      wibox.widget.systray(),
      s.mylayoutbox
    },
  }
  -- if has battery then add battery widget
  awful.spawn.easy_async_with_shell("acpi -b", function(c) 
    if c:find("Battery") then s.mywibox:get_children_by_id("rightwidgets")[1]:insert(3, widgets.bat) end
  end)
end)
-- }}}


-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
  -- Start the new client as slave.
  if not awesome.startup then
    awful.client.setslave(c)
  end

  if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c, { honor_padding = true })
  end
end)


-- Turn border color to back if there is only one client in the tag.
function border_adjust(c)
  if #awful.screen.focused().clients == 1 then
    c.border_color = beautiful.border_normal
  else
    c.border_color = beautiful.border_focus
  end
end


-- Force minimized clients to unminimize.
client.connect_signal("property::minimized", function(c)
  c.minimized = false
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = gears.table.join(
      awful.button({ }, 1, function()
        client.focus = c
        c:raise()
        awful.mouse.client.move(c)
      end),
      awful.button({ }, 3, function()
        client.focus = c
        c:raise()
        awful.mouse.client.resize(c)
      end)
  )

  awful.titlebar(c):setup {
    { -- Left
      awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Title
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
      awful.titlebar.widget.floatingbutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.stickybutton(c),
      awful.titlebar.widget.ontopbutton(c),
      awful.titlebar.widget.closebutton(c),
      layout = wibox.layout.fixed.horizontal()
    },
    layout = wibox.layout.align.horizontal
  }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
  if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
      and awful.client.focus.filter(c) then
    client.focus = c
  end
end)

client.connect_signal("focus", border_adjust)
client.connect_signal("unfocus", function(c)
  c.border_color = beautiful.border_normal
end)

awful.spawn.with_shell("~/.config/awesome/autorun.sh")
