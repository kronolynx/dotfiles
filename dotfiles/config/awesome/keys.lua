local awful         = require("awful")
local gears         = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
--require("awful.hotkeys_popup.keys")
require("variables")

local modkey   = "Mod4" -- Super
local altkey   = "Mod1" -- Alt
local ctrlkey  = "Control"
local shiftkey = "Shift"


-- {{{ Key bindings
globalkeys     = gears.table.join(
-- Move between tags
    awful.key({ modkey }, "Tab", awful.tag.history.restore, { description = "go back", group = "tag" }),
-- Focus clients by direction
    awful.key({ modkey }, "Down", function()
      awful.client.focus.global_bydirection("down")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus down", group = "client" }
    ),
    awful.key({ modkey }, "Up", function()
      awful.client.focus.global_bydirection("up")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus up", group = "client" }
    ),
    awful.key({ modkey }, "Left", function()
      awful.client.focus.global_bydirection("left")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus left", group = "client" }
    ),
    awful.key({ modkey }, "Right", function()
      awful.client.focus.global_bydirection("right")
     if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus right", group = "client" }
    ),
-- By direction client focus
    awful.key({ modkey }, "j", function()
      awful.client.focus.global_bydirection("down")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus down", group = "client" }
    ),
    awful.key({ modkey }, "k", function()
      awful.client.focus.global_bydirection("up")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus up", group = "client" }
    ),
    awful.key({ modkey }, "h", function()
      awful.client.focus.global_bydirection("left")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus left", group = "client" }
    ),
    awful.key({ modkey }, "l", function()
      awful.client.focus.global_bydirection("right")
      if client.focus then
        client.focus:raise()
      end
    end,
        { description = "focus right", group = "client" }
    ),
-- Focus client by index (cycle through clients)
    awful.key({ altkey }, "Tab", function() awful.client.focus.byidx(1) end,
        { description = "cycle through clients", group = "client" }
    ),
-- Layout manipulation
    awful.key({ modkey, shiftkey }, "Left", function() awful.client.swap.global_bydirection("left") end,
        { description = "swap with direction left", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "Right", function() awful.client.swap.global_bydirection("right") end,
        { description = "swap with direction right", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "Up", function() awful.client.swap.global_bydirection("up") end,
        { description = "swap with direction up", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "Down", function() awful.client.swap.global_bydirection("down") end,
        { description = "swap with direction down", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "h", function() awful.client.swap.global_bydirection("left") end,
        { description = "swap with direction left", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "l", function() awful.client.swap.global_bydirection("right") end,
        { description = "swap with direction right", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "k", function() awful.client.swap.global_bydirection("up") end,
        { description = "swap with direction up", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "j", function() awful.client.swap.global_bydirection("down") end,
        { description = "swap with direction down", group = "client" }
    ),
    -- Urgent or Undo:
    -- Jump to urgent client or (if there is no such client) go back
    -- to the last tag
    awful.key({ modkey }, "u", function() 
      if awful.client.urgent.get() then 
        awful.client.urgent.jumpto()
      else 
        awful.tag.history.restore()
      end
    end, { description = "jump to urgent client/last window", group = "client" }
    ),
-- Show/Hide Wibox
    awful.key({ modkey }, "b", function()
      for s in screen do
        s.mywibox.visible = not s.mywibox.visible
        if s.mybottomwibox then
          s.mybottomwibox.visible = not s.mybottomwibox.visible
        end
      end
    end,
        { description = "toggle wibox", group = "awesome" }
    ),
-- Move between screens
    awful.key({ modkey, }, "s", function() awful.screen.focus_relative(1) end,
        { description = "focus the next screen", group = "screen" }
    ),
    awful.key({ altkey }, "Left", function() awful.screen.focus_relative(1) end,
        { description = "focus the next screen", group = "screen" }
    ),
    awful.key({ altkey }, "Right", function() awful.screen.focus_relative(-1) end,
        { description = "focus the previous screen", group = "screen" }
    ),
    awful.key({ modkey, shiftkey }, "o", function()
      if screen:count() == 2 then
        local current_screen    = awful.screen.focused()
        local next_screen_index = (current_screen.index % 2) + 1
        local next_screen       = screen[next_screen_index]
        local next_tag          = next_screen.selected_tag
        if next_tag then
          local current_tag = current_screen.selected_tag
          local clients     = current_screen.clients
          local tag_clients = next_tag:clients()
          for _, c in pairs(clients) do
            c:move_to_tag(next_tag)
          end
          if current_tag then
            for _, c in pairs(tag_clients) do
              c:move_to_tag(current_tag)
            end
          end
        end
      end
    end,
       { description = "swap tag clients with the tag on the next screen", group = "tag" }
    ),
-- Standard program
    awful.key({ modkey }, "Return", function() awful.spawn(terminal) end,
        { description = "open a terminal", group = "apps" }
    ),
    awful.key({ modkey }, "F3", function() awful.spawn(browser1) end,
        { description = "open " .. browser1, group = "apps" }
    ),
    awful.key({ modkey, shiftkey }, "Return", function() awful.spawn(file1) end,
        { description = "open main file manager", group = "apps" }
    ),
    awful.key({ modkey, ctrlkey, shiftkey }, "Return", function() awful.spawn(file2) end,
        { description = "open secondary file manager", group = "apps" }
    ),
-- Awesome actions
    awful.key({ modkey, ctrlkey }, "r", awesome.restart,
        { description = "reload awesome", group = "awesome" }
    ),
    awful.key({ modkey, shiftkey }, "s", hotkeys_popup.show_help,
        { description = "show help", group = "awesome" }
    ),
    awful.key({ modkey, }, ".", function() awful.tag.incmwfact(0.05) end,
        { description = "increase master width factor", group = "layout" }
    ),
    awful.key({ modkey, }, ",", function() awful.tag.incmwfact(-0.05) end,
        { description = "decrease master width factor", group = "layout" }
    ),
    awful.key({ modkey, ctrlkey }, ".", function() awful.tag.incnmaster(1, nil, true) end,
        { description = "increase the number of master clients", group = "layout" }
    ),
    awful.key({ modkey, ctrlkey }, ",", function() awful.tag.incnmaster(-1, nil, true) end,
        { description = "decrease the number of master clients", group = "layout" }
    ),
    awful.key({ altkey, ctrlkey }, ".", function() awful.tag.incncol(1, nil, true) end,
        { description = "increase the number of columns", group = "layout" }
    ),
    awful.key({ altkey, ctrlkey }, ",", function() awful.tag.incncol(-1, nil, true) end,
        { description = "decrease the number of columns", group = "layout" }
    ),
    awful.key({ modkey }, "space", function() awful.layout.inc(1) end,
        { description = "select next", group = "layout" }
    ),
    awful.key({ modkey, shiftkey }, "space", function() awful.layout.inc(-1) end,
        { description = "select previous", group = "layout" }
    ),

    awful.key({ modkey }, "r", function() awful.screen.focused().mypromptbox:run() end,
        { description = "run prompt", group = "launcher" }
    ),
    awful.key({ modkey }, "Escape", function ()
        exit_screen_show()
    end,
        {description = "logout menu", group = "awesome"}
    ),
    awful.key({ modkey }, "x", function()
      awful.prompt.run {
        prompt       = "Run Lua code: ",
        textbox      = awful.screen.focused().mypromptbox.widget,
        exe_callback = awful.util.eval,
        history_path = awful.util.get_cache_dir() .. "/history_eval"
      }
    end,
        { description = "lua execute prompt", group = "awesome" }
    ),
    awful.key({ ctrlkey }, "Escape", function() awful.spawn("xkill") end,
        { desc = "launch xkill", group = "client" }
    ),
-- Kill all visible clients for the current tag
    awful.key({ modkey, altkey }, "q", function()
      local clients = awful.screen.focused().clients
      for _, c in pairs(clients) do
        c:kill()
      end
    end,
        { description = "kill all visible clients for the current tag", group = "client" }
    ),
-- Launcher
    awful.key({ altkey }, "d", function() awful.spawn.with_shell(my_launcher) end,
        { description = "run launcher", group = "launcher" }
    ),

-- Volume Control
    awful.key({}, "XF86AudioMute", function() awful.spawn.with_shell("pulsemixer --toggle-mute") end,
        { description = "mute/unmute volume", group = "controls" }
    ),
    awful.key({}, "XF86AudioLowerVolume", function() awful.spawn.with_shell("pulsemixer --change-volume -5") end,
        { description = "lower volume", group = "controls" }
    ),
    awful.key({}, "XF86AudioRaiseVolume", function() awful.spawn.with_shell("pulsemixer --change-volume +5") end,
        { description = "raise volume", group = "controls" }
    ),
-- Media Controls
    awful.key({}, "XF86AudioPlay", function() awful.spawn.with_shell("playerctl play-pause") end,
        { description = "toggle play/pause", group = "controls" }
    ),
    awful.key({}, "XF86AudioStop", function() awful.spawn.with_shell("playerctl stop") end,
        { description = "stops music", group = "controls" }
    ),
    awful.key({}, "XF86AudioNext", function() awful.spawn.with_shell("playerctl next") end,
        { description = "next song", group = "controls" }
    ),
    awful.key({}, "XF86AudioPrev", function() awful.spawn.with_shell("playerctl previous") end,
        { description = "previous song", group = "controls" }
    ),
-- Screen Brightness
    awful.key({}, "XF86MonBrightnessUp", function()
      awful.spawn.with_shell('xbacklight + 3 -time 100 -steps 1 && notify-send "brightness up $(xbacklight -get)"')
    end,
        { description = "Increase brightness", group = "controls" }
    ),
    awful.key({}, "XF86MonBrightnessDown", function()
      awful.spawn.with_shell('xbacklight - 3 -time 100 -steps 1 && notify-send "brightness down $(xbacklight -get)"')
    end,
        { description = "Decrease brightness", group = "controls" }
    ),
-- Screenshot
    awful.key({}, "Print", function()
      awful.spawn.with_shell(my_screen_capture .. " && notify-send 'Desktop captured'")
    end,
        { description = "take a screenshot of entire screen", group = "screenshot" }
    ),
    awful.key({ ctrlkey }, "Print", function()
      awful.spawn.with_shell(my_screen_capture .. " -u && notify-send 'Focused window captured'")
    end,
        { description = "take a screenshot of focused window", group = "screenshot" }
    ),
    awful.key({ shiftkey }, "Print", function()
      awful.spawn.with_shell("notify-send 'Select Area';sleep 0.2;" .. my_screen_capture .. " -s && notify-send 'Area captured'")
    end,
        { description = "take a screenshot of selected area", group = "screenshot" }
    )
)

clientkeys     = gears.table.join(
    awful.key(
        { modkey, shiftkey }, "f", function(c)
          c.fullscreen = not c.fullscreen
          c:raise()
        end,
        { description = "toggle fullscreen", group = "client" }
    ),
    awful.key({ modkey }, "f", function(c)
      c.maximized = not c.maximized
      c:raise()
    end,
        { description = "(un)maximize", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, "q", function(c) c:kill() end,
        { description = "close", group = "client" }
    ),
    awful.key({ modkey, ctrlkey }, "space", awful.client.floating.toggle,
        { description = "toggle floating", group = "client" }
    ),
    awful.key({ modkey }, "m", function(c) c:swap(awful.client.getmaster()) end,
        { description = "move to master", group = "client" }
    ),
    awful.key({ modkey }, "o", function(c) c:move_to_screen() end,
        { description = "move to screen", group = "client" }
    ),
    awful.key({ modkey }, "t", function(c) c.ontop = not c.ontop end,
        { description = "toggle keep on top", group = "client" }
    ),
    awful.key({ modkey, ctrlkey }, "t", function(c) awful.titlebar.toggle(c) end,
        { description = "toggle title bar", group = "client" }
    ),
-- Slave client resize
    awful.key({ modkey, shiftkey }, ",", function(c) awful.client.incwfact(0.05, c) end,
        { description = "increase client size", group = "client" }
    ),
    awful.key({ modkey, shiftkey }, ".", function(c) awful.client.incwfact(-0.05, c) end,
        { description = "decrease client size", group = "client" }
    ),
    awful.key({ modkey }, "n", function(c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
    end,
        { description = "minimize", group = "client" }
    ),
    awful.key({ modkey, ctrlkey }, "n", function()
      local c = awful.client.restore()
      -- Focus restored client
      if c then
        c:emit_signal("request::activate", "key.unminimize", { raise = true })
      end
    end,
        { description = "restore minimized", group = "client" })
)


-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 10 do
  globalkeys = gears.table.join(
      globalkeys,
  -- View tag only.
      awful.key({ modkey }, "#" .. i + 9, function()
        local screen = awful.screen.focused()
        local tag    = screen.tags[i]
        if tag then
          tag:view_only()
        end
      end,
          { description = "view tag", group = "tag" }
      ),
  -- Move client to tag.
      awful.key({ modkey, ctrlkey }, "#" .. i + 9, function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
          { description = "move focused client to tag", group = "tag" }
      ),
  -- Toggle tag display.
      awful.key({ modkey, altkey }, "#" .. i + 9, function()
        local screen = awful.screen.focused()
        local tag    = screen.tags[i]
        if tag then
          awful.tag.viewtoggle(tag)
        end
      end,
          { description = "toggle tag", group = "tag" }
      ),
  -- Move all visible clients to tag and focus that tag
      awful.key({ modkey, shiftkey, altkey }, "#" .. i + 9, function()
        local tag     = client.focus.screen.tags[i]
        local clients = awful.screen.focused().clients
        if tag then
          for _, c in pairs(clients) do
            c:move_to_tag(tag)
          end
          tag:view_only()
        end
      end,
          { description = "move all visible clients to tag", group = "tag" }
      ),
  -- Greedy view ,Move client to tag and view tag
      awful.key({ modkey, shiftkey }, "#" .. i + 9, function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:move_to_tag(tag)
            tag:view_only()
          end
        end
      end,
          { description = "move focused client to tag and view tag", group = "tag" }
      ),
  -- Toggle tag on focused client.
      awful.key({ modkey, shiftkey, ctrlkey }, "#" .. i + 9, function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then
            client.focus:toggle_tag(tag)
          end
        end
      end,
          { description = "toggle focused client on tag", group = "tag" }
      ),
      awful.key({ altkey, shiftkey }, "#" .. i + 9, function()
        local current_screen = awful.screen.focused()
        local tag            = current_screen.tags[i]
        local clients        = current_screen.clients
        local current_tag    = current_screen.selected_tag
        if tag then
          local tag_clients = tag:clients()
          for _, c in pairs(clients) do
            c:move_to_tag(tag)
          end
          if current_tag then
            for _, c in pairs(tag_clients) do
              c:move_to_tag(current_tag)
            end
          end
        end
      end,
          { description = "swap current tag with tag", group = "tag" }
      )
  )
end
-- }}}

clientbuttons              = gears.table.join(
    awful.button({ }, 1, function(c)
      c:emit_signal("request::activate", "mouse_click", { raise = true })
    end),
    awful.button({ modkey }, 1, function(c)
      -- left click
      c:emit_signal("request::activate", "mouse_click", { raise = true })
      awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function(c)
      -- right click
      c:emit_signal("request::activate", "mouse_click", { raise = true })
      awful.mouse.client.resize(c)
    end)
)

awful.util.taglist_buttons = gears.table.join(
    awful.button({}, 1, function(t) t:view_only() end),
    awful.button({}, 3, awful.tag.viewtoggle)
)

clientbuttons_jetbrains    = gears.table.join(
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}

root.keys(globalkeys)
