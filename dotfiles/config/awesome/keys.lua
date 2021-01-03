--      ██╗  ██╗███████╗██╗   ██╗███████╗
--      ██║ ██╔╝██╔════╝╚██╗ ██╔╝██╔════╝
--      █████╔╝ █████╗   ╚████╔╝ ███████╗
--      ██╔═██╗ ██╔══╝    ╚██╔╝  ╚════██║
--      ██║  ██╗███████╗   ██║   ███████║
--      ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝


-- ===================================================================
-- Initialization
-- ===================================================================

local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup").widget

require("variables")
local helpers = require("helpers")
local inspect = require("inspect")

local keys = {}

local modkey = "Mod4" -- Super
local altkey = "Mod1" -- Alt
local ctrlkey = "Control"
local shiftkey = "Shift"


-- ===================================================================
-- Desktop Key bindings
-- ===================================================================

-- {{{ Key bindings
keys.globalkeys =
  gears.table.join(
  -- Move between tags
  awful.key({modkey}, "Tab", awful.tag.history.restore, {description = "go back", group = "tag"}),

  -- =========================================
  -- CLIENT FOCUSING
  -- =========================================

  -- Focus client by direction (arrow keys)
  awful.key(
    {modkey},
    "Down",
    function()
      awful.client.focus.global_bydirection("down")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus down", group = "client"}
  ),
  awful.key(
    {modkey},
    "Up",
    function()
      awful.client.focus.global_bydirection("up")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus up", group = "client"}
  ),
  awful.key(
    {modkey},
    "Left",
    function()
      awful.client.focus.global_bydirection("left")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus left", group = "client"}
  ),
  awful.key(
    {modkey},
    "Right",
    function()
      awful.client.focus.global_bydirection("right")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus right", group = "client"}
  ),

  -- Focus client by direction (hjkl keys)
  awful.key(
    {modkey},
    "j",
    function()
      awful.client.focus.global_bydirection("down")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus down", group = "client"}
  ),
  awful.key(
    {modkey},
    "k",
    function()
      awful.client.focus.global_bydirection("up")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus up", group = "client"}
  ),
  awful.key(
    {modkey},
    "h",
    function()
      awful.client.focus.global_bydirection("left")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus left", group = "client"}
  ),
  awful.key(
    {modkey},
    "l",
    function()
      awful.client.focus.global_bydirection("right")
      if client.focus then
        client.focus:raise()
      end
    end,
    {description = "focus right", group = "client"}
  ),

  -- Window switcher
  awful.key(
    {altkey},
    "Tab",
    function()
      -- awful.client.focus.byidx(1)
      window_switcher_show(awful.screen.focused())
    end,
    {description = "cycle through clients", group = "client"}
  ),
  -- Urgent or Undo:
  -- Jump to urgent client or (if there is no such client) go back
  -- to the last tag
  awful.key(
    {modkey},
    "u",
    function()
      if awful.client.urgent.get() then
        awful.client.urgent.jumpto()
      else
        awful.tag.history.restore()
      end
    end,
    {description = "jump to urgent client/last window", group = "client"}
  ),
  -- Show/Hide Wibox
  awful.key(
    {modkey},
    "b",
    function()
      for s in screen do
        s.mywibox.visible = not s.mywibox.visible
        if s.mybottomwibox then
          s.mybottomwibox.visible = not s.mybottomwibox.visible
        end
      end
    end,
    {description = "toggle wibox", group = "awesome"}
  ),
  -- Show/Hide tray
  awful.key(
    {modkey, shiftkey},
    "b",
    function()
      awful.screen.focused().systray.visible = not awful.screen.focused().systray.visible
    end,
    {description = "toggle wibox", group = "awesome"}
  ),
  -- Move between screens
  awful.key(
    {modkey},
    "s",
    function()
      awful.screen.focus_relative(1)
    end,
    {description = "focus the next screen", group = "screen"}
  ),
  awful.key(
    {altkey},
    "Left",
    function()
      awful.screen.focus_relative(1)
    end,
    {description = "focus the next screen", group = "screen"}
  ),
  awful.key(
    {altkey},
    "Right",
    function()
      awful.screen.focus_relative(-1)
    end,
    {description = "focus the previous screen", group = "screen"}
  ),
  awful.key(
    {modkey, shiftkey},
    "o",
    function()
      if screen:count() == 2 then
        local current_screen = awful.screen.focused()
        local next_screen_index = (current_screen.index % 2) + 1
        local next_screen = screen[next_screen_index]
        local next_tag = next_screen.selected_tag
        if next_tag then
          local current_tag = current_screen.selected_tag
          local clients = current_screen.clients
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
    {description = "swap tag clients with the tag on the next screen", group = "tag"}
  ),
  awful.key(
    {modkey},
    "t",
    function()
      local clients = awful.screen.focused().clients
      for _, c in pairs(clients) do
        awful.titlebar.toggle(c)
      end
    end,
    {description = "toggle title bars for clients in current tag", group = "client"}
  ),
  -- Awesome actions
  awful.key({modkey, shiftkey}, "s", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),

   -- =========================================
   -- NUMBER OF MASTER / COLUMN CLIENTS
   -- =========================================
  awful.key(
    {modkey},
    ".",
    function()
      awful.tag.incmwfact(0.05)
    end,
    {description = "increase master width factor", group = "layout"}
  ),
  awful.key(
    {modkey},
    ",",
    function()
      awful.tag.incmwfact(-0.05)
    end,
    {description = "decrease master width factor", group = "layout"}
  ),
  awful.key(
    {modkey, ctrlkey},
    ".",
    function()
      awful.tag.incnmaster(1, nil, true)
    end,
    {description = "increase the number of master clients", group = "layout"}
  ),
  awful.key(
    {modkey, ctrlkey},
    ",",
    function()
      awful.tag.incnmaster(-1, nil, true)
    end,
    {description = "decrease the number of master clients", group = "layout"}
  ),
  awful.key(
    {altkey, ctrlkey},
    ".",
    function()
      awful.tag.incncol(1, nil, true)
    end,
    {description = "increase the number of columns", group = "layout"}
  ),
  awful.key(
    {altkey, ctrlkey},
    ",",
    function()
      awful.tag.incncol(-1, nil, true)
    end,
    {description = "decrease the number of columns", group = "layout"}
  ),
  awful.key(
    {modkey},
    "space",
    function()
      awful.layout.inc(1)
    end,
    {description = "select next", group = "layout"}
  ),

  -- =========================================
  -- GAP CONTROL
  -- =========================================

  -- Gap control
  awful.key({modkey, "Shift"}, "minus",
    function()
        awful.tag.incgap(5, nil)
    end,
    {description = "increment gaps size for the current tag", group = "gaps"}
  ),
  awful.key({modkey}, "minus",
    function()
        awful.tag.incgap(-5, nil)
    end,
    {description = "decrement gap size for the current tag", group = "gaps"}
  ),

  -- =========================================
  -- LAYOUT SELECTION
  -- =========================================

  -- select next layout
  awful.key({modkey}, "space",
    function()
        awful.layout.inc(1)
    end,
    {description = "select next", group = "layout"}
  ),
  -- select previous layout
  awful.key({modkey, "Shift"}, "space",
    function()
        awful.layout.inc(-1)
    end,
    {description = "select previous", group = "layout"}
  ),


  awful.key(
    {modkey},
    "x",
    function()
      awful.prompt.run {
        prompt = "Run Lua code: ",
        textbox = awful.screen.focused().mypromptbox.widget,
        exe_callback = awful.util.eval,
        history_path = awful.util.get_cache_dir() .. "/history_eval"
      }
    end,
    {description = "lua execute prompt", group = "awesome"}
  ),
  awful.key(
    {ctrlkey},
    "Escape",
    function()
      awful.spawn("xkill")
    end,
    {desc = "launch xkill", group = "client"}
  ),
  -- Kill all visible clients for the current tag
  awful.key(
    {modkey, altkey},
    "q",
    function()
      local clients = awful.screen.focused().clients
      for _, c in pairs(clients) do
        c:kill()
      end
    end,
    {description = "kill all visible clients for the current tag", group = "client"}
  ),

  awful.key(
    {modkey},
    "r",
    function()
      awful.screen.focused().mypromptbox:run()
    end,
    {description = "run prompt", group = "launcher"}
  ),

  -- rofi window selector
  awful.key(
    {modkey},
    "g",
    function()
      awful.spawn.with_shell(user.window_selector)
    end,
    {description = "window selector", group = "awesome"}
  ),

  -- =========================================
  -- RELOAD / QUIT AWESOME
  -- =========================================
  awful.key({modkey, ctrlkey}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
  
  awful.key(
    {modkey},
    "Escape",
    function()
      exit_screen_show()
    end,
    {description = "select window", group = "awesome"}
  ),

  awful.key({}, "XF86PowerOff",
    function()
      exit_screen_show()
    end,
    {description = "toggle exit screen", group = "hotkeys"}
  ),

  -- =========================================
  -- SPAWN APPLICATION KEY BINDINGS
  -- =========================================

  -- Standard program
  awful.key(
    {modkey},
    "Return",
    function()
      awful.spawn(user.terminal)
    end,
    {description = "open a terminal", group = "apps"}
  ),
  awful.key(
    {modkey},
    "F3",
    function()
      awful.spawn(user.browser)
    end,
    {description = "open " .. user.browser, group = "apps"}
  ),
  awful.key(
    {modkey, shiftkey},
    "Return",
    function()
      awful.spawn(user.file1)
    end,
    {description = "open main file manager", group = "apps"}
  ),
  awful.key(
    {modkey, ctrlkey, shiftkey},
    "Return",
    function()
      awful.spawn(user.file2)
    end,
    {description = "open secondary file manager", group = "apps"}
  ),
  awful.key(
    {modkey, ctrlkey},
    "Return",
    function()
      awful.spawn(user.editor)
    end,
    {description = "open editor", group = "apps"}
  ),
  -- Launcher
  awful.key(
    {altkey},
    "d",
    function()
      awful.spawn.with_shell(user.launcher)
    end,
    {description = "run launcher", group = "launcher"}
  ),

  -- =========================================
  -- FUNCTION KEYS
  -- =========================================

  -- Volume Control
  awful.key(
    {},
    "XF86AudioMute",
    function()
      awful.spawn.with_shell("pulsemixer --toggle-mute")
    end,
    {description = "mute/unmute volume", group = "controls"}
  ),
  awful.key(
    {},
    "XF86AudioLowerVolume",
    function()
      awful.spawn.with_shell("pulsemixer --change-volume -5")
    end,
    {description = "lower volume", group = "controls"}
  ),
  awful.key(
    {},
    "XF86AudioRaiseVolume",
    function()
      awful.spawn.with_shell("pulsemixer --change-volume +5")
    end,
    {description = "raise volume", group = "controls"}
  ),
  -- Media Controls
  awful.key(
    {},
    "XF86AudioPlay",
    function()
      awful.spawn.with_shell("playerctl play-pause")
    end,
    {description = "toggle play/pause", group = "controls"}
  ),
  awful.key(
    {},
    "XF86AudioStop",
    function()
      awful.spawn.with_shell("playerctl stop")
    end,
    {description = "stops music", group = "controls"}
  ),
  awful.key(
    {},
    "XF86AudioNext",
    function()
      awful.spawn.with_shell("playerctl next")
    end,
    {description = "next song", group = "controls"}
  ),
  awful.key(
    {},
    "XF86AudioPrev",
    function()
      awful.spawn.with_shell("playerctl previous")
    end,
    {description = "previous song", group = "controls"}
  ),
  -- Screen Brightness
  awful.key(
    {},
    "XF86MonBrightnessUp",
    function()
      awful.spawn.with_shell('xbacklight + 3 -time 100 -steps 1 && notify-send "brightness up $(xbacklight -get)"')
    end,
    {description = "Increase brightness", group = "controls"}
  ),
  awful.key(
    {},
    "XF86MonBrightnessDown",
    function()
      awful.spawn.with_shell('xbacklight - 3 -time 100 -steps 1 && notify-send "brightness down $(xbacklight -get)"')
    end,
    {description = "Decrease brightness", group = "controls"}
  ),
  -- Screenshot
  awful.key(
    {},
    "Print",
    function()
      awful.spawn.with_shell(user.screen_capture_root .. " && notify-send 'Desktop captured'")
    end,
    {description = "take a screenshot of entire screen", group = "screenshot"}
  ),
  awful.key(
    {ctrlkey},
    "Print",
    function()
      awful.spawn.with_shell(user.screen_capture_window .. " && notify-send 'Focused window captured'")
    end,
    {description = "take a screenshot of focused window", group = "screenshot"}
  ),
  awful.key(
    {shiftkey},
    "Print",
    function()
      awful.spawn.with_shell(
        "notify-send 'Select Area';sleep 0.2;" .. user.screen_capture_area .. " && notify-send 'Area captured'"
      )
    end,
    {description = "take a screenshot of selected area", group = "screenshot"}
  ),
  awful.key( {modkey}, "'", function()
      local grabber
      grabber =
        awful.keygrabber.run(
        function(_, key, event)
          if event == "release" then return end

          if key == "b" then awful.spawn(user.browser)
          elseif key == "e" then awful.spawn(user.editor)
          elseif key == "f" then awful.spawn("firefox")
          elseif key == "g" then awful.spawn("google-chrome-stable")
          elseif key == "j" then awful.spawn("joplin")
          elseif key == "k" then awful.spawn("xkill")
          elseif key == "m" then awful.spawn(user.music)
          elseif key == "p" then awful.spawn("postman")
          elseif key == "r" then awful.spawn(user.terminal .. " -e ranger")
          elseif key == "s" then awful.spawn("slack")
          end
          awful.keygrabber.stop(grabber)
        end
      )
    end,
    {description = "launchers", group = "misc"}
  )
)


keys.clientkeys =
  gears.table.join(
  awful.key(
    {modkey},
    "f",
    function(c)
      helpers.toggle_full(c)
    end,
    {description = "(un)maximize", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "f",
    function(c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end,
    {description = "toggle clients fullscreen", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "q",
    function(c)
      c:kill()
    end,
    {description = "close", group = "client"}
  ),
  awful.key(
    {modkey, ctrlkey},
    "space",
    awful.client.floating.toggle,
    {description = "toggle floating", group = "client"}
  ),
  awful.key(
    {modkey},
    "m",
    function(c)
      c:swap(awful.client.getmaster())
    end,
    {description = "move to master", group = "client"}
  ),
  awful.key(
    {modkey},
    "o",
    function(c)
      c:move_to_screen()
    end,
    {description = "move to screen", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "t",
    function(c)
      awful.titlebar.toggle(c)
    end,
    {description = "toggle title bar", group = "client"}
  ),
  awful.key(
    {modkey, ctrlkey},
    "t",
    function(c)
      c.ontop = not c.ontop
    end,
    {description = "toggle keep on top", group = "client"}
  ),

  -- =========================================
  -- CLIENT RESIZING
  -- =========================================

  awful.key({modkey, "Control"}, "Down",
    function(c)
        helpers.resize_client(client.focus, "down")
    end
  ),
  awful.key({modkey, "Control"}, "Up",
    function(c)
        helpers.resize_client(client.focus, "up")
    end
  ),
  awful.key({modkey, "Control"}, "Left",
    function(c)
        helpers.resize_client(client.focus, "left")
    end
  ),
  awful.key({modkey, "Control"}, "Right",
    function(c)
        helpers.resize_client(client.focus, "right")
    end
  ),
  awful.key({modkey, "Control"}, "j",
    function(c)
        helpers.resize_client(client.focus, "down")
    end
  ),
  awful.key({ modkey, "Control" }, "k",
    function(c)
        helpers.resize_client(client.focus, "up")
    end
  ),
  awful.key({modkey, "Control"}, "h",
    function(c)
        helpers.resize_client(client.focus, "left")
    end
  ),
  awful.key({modkey, "Control"}, "l",
    function(c)
        helpers.resize_client(client.focus, "right")
    end
  ),

  -- =========================================
  -- CLIENT MINIMIZATION
  -- =========================================
  awful.key(
    {modkey},
    "n",
    function(c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
    end,
    {description = "minimize", group = "client"}
  ),
  awful.key(
    {modkey, ctrlkey},
    "n",
    function()
      local c = awful.client.restore()
      -- Focus restored client
      if c then
        c:emit_signal("request::activate", "key.unminimize", {raise = true})
      end
    end,
    {description = "restore minimized", group = "client"}
  ),
  -- Single tap: Center client
  -- Double tap: Center client + Floating + Resize
  awful.key(
    {modkey},
    "c",
    function(c)
      awful.placement.centered(c, {honor_workarea = true, honor_padding = true})
      helpers.single_double_tap(
        nil,
        function()
          helpers.float_and_resize(c, screen_width * 0.65, screen_height * 0.9)
        end
      )
    end
  ),
  -- Move to edge or swap by direction
  awful.key(
    {modkey, shiftkey},
    "Down",
    function(c)
      helpers.move_client_dwim(c, "down")
    end,
    {description = "move down", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "Up",
    function(c)
      helpers.move_client_dwim(c, "up")
    end,
    {description = "move up", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "Left",
    function(c)
      helpers.move_client_dwim(c, "left")
    end,
    {description = "move left", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "Right",
    function(c)
      helpers.move_client_dwim(c, "right")
    end,
    {description = "move right", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "j",
    function(c)
      helpers.move_client_dwim(c, "down")
    end,
    {description = "move down", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "k",
    function(c)
      helpers.move_client_dwim(c, "up")
    end,
    {description = "move up", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "h",
    function(c)
      helpers.move_client_dwim(c, "left")
    end,
    {description = "move left", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey},
    "l",
    function(c)
      helpers.move_client_dwim(c, "right")
    end,
    {description = "move right", group = "client"}
  ),
  -- Single tap: Center client
  -- Double tap: Center client + Floating + Resize
  awful.key(
    {modkey},
    "c",
    function(c)
      awful.placement.centered(c, {honor_workarea = true, honor_padding = true})
      helpers.single_double_tap(
        nil,
        function()
          helpers.float_and_resize(c, screen_width * 0.65, screen_height * 0.9)
        end
      )
    end
  ),
  -- Relative move client
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "j",
    function(c)
      c:relative_move(0, dpi(20), 0, 0)
    end,
    {description = "move relative down", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "k",
    function(c)
      c:relative_move(0, dpi(-20), 0, 0)
    end,
    {description = "move relative up", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "h",
    function(c)
      c:relative_move(dpi(-20), 0, 0, 0)
    end,
    {description = "move relative left", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "l",
    function(c)
      c:relative_move(dpi(20), 0, 0, 0)
    end,
    {description = "move relative right", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "Down",
    function(c)
      c:relative_move(0, dpi(20), 0, 0)
    end,
    {description = "move relative down", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "Up",
    function(c)
      c:relative_move(0, dpi(-20), 0, 0)
    end,
    {description = "move relative up", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "Left",
    function(c)
      c:relative_move(dpi(-20), 0, 0, 0)
    end,
    {description = "move relative left", group = "client"}
  ),
  awful.key(
    {modkey, shiftkey, ctrlkey},
    "Right",
    function(c)
      c:relative_move(dpi(20), 0, 0, 0)
    end,
    {description = "move relative right", group = "client"}
  )
)

local function addTagKey(tagKey, tagNumber, keys)
  return gears.table.join(
    keys,
    -- View tag only.
    awful.key(
      {modkey},
      tagKey,
      function()
        helpers.tag_back_and_forth(tagNumber)
      end,
      {description = "view tag", group = "tag"}
    ),
    -- Move client to tag.
    awful.key(
      {modkey, ctrlkey},
      tagKey,
      function()
        if client.focus then
          local tag = client.focus.screen.tags[tagNumber]
          if tag then
            client.focus:move_to_tag(tag)
          end
        end
      end,
      {description = "move focused client to tag", group = "tag"}
    ),
    -- Toggle tag display.
    awful.key(
      {modkey, altkey},
      tagKey,
      function()
        local screen = awful.screen.focused()
        local tag = screen.tags[tagNumber]
        if tag then
          awful.tag.viewtoggle(tag)
        end
      end,
      {description = "toggle tag", group = "tag"}
    ),
    -- Move all visible clients to tag and focus that tag
    awful.key(
      {modkey, shiftkey, altkey},
      tagKey,
      function()
        local tag = client.focus.screen.tags[tagNumber]
        local clients = awful.screen.focused().clients
        if tag then
          for _, c in pairs(clients) do
            c:move_to_tag(tag)
          end
          tag:view_only()
        end
      end,
      {description = "move all visible clients to tag", group = "tag"}
    ),
    -- Greedy view ,Move client to tag and view tag
    awful.key(
      {modkey, shiftkey},
      tagKey,
      function()
        if client.focus then
          local tag = client.focus.screen.tags[tagNumber]
          if tag then
            client.focus:move_to_tag(tag)
            tag:view_only()
          end
        end
      end,
      {description = "move focused client to tag and view tag", group = "tag"}
    ),
    -- Toggle tag on focused client.
    awful.key(
      {modkey, shiftkey, ctrlkey},
      tagKey,
      function()
        if client.focus then
          local tag = client.focus.screen.tags[tagNumber]
          if tag then
            client.focus:toggle_tag(tag)
          end
        end
      end,
      {description = "toggle focused client on tag", group = "tag"}
    ),
    awful.key(
      {altkey, shiftkey},
      tagKey,
      function()
        local current_screen = awful.screen.focused()
        local tag = current_screen.tags[tagNumber]
        local clients = current_screen.clients
        local current_tag = current_screen.selected_tag
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
      {description = "swap current tag with tag", group = "tag"}
    )
  )
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 10 do
  keys.globalkeys = addTagKey("#" .. i + 9, i, keys.globalkeys)
end

keys.globalkeys = addTagKey("=", 11, keys.globalkeys)
keys.globalkeys = addTagKey("-", 12, keys.globalkeys)
keys.globalkeys = addTagKey("[", 13, keys.globalkeys)
keys.globalkeys = addTagKey("]", 14, keys.globalkeys)
-- }}}


-- ===================================================================
-- Mouse bindings
-- ===================================================================


-- Mouse buttons on the desktop
keys.desktopbuttons = gears.table.join(
   -- left click on desktop to hide notification
   awful.button({}, 1,
      function ()
         naughty.destroy_all_notifications()
      end
   )
)

keys.clientbuttons =
  gears.table.join(
  awful.button(
    {},
    1,
    function(c)
      c:emit_signal("request::activate", "mouse_click", {raise = true})
    end
  ),
  awful.button(
    {modkey},
    1,
    function(c)
      -- left click
      if c.floating then
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        if c.maximized then
          c.maximized = false
        end
        awful.mouse.client.move(c)
      else
        c:swap(awful.client.getmaster())
      end
    end
  ),
  awful.button(
    {modkey},
    3,
    function(c)
      -- right click
      c:emit_signal("request::activate", "mouse_click", {raise = true})
      awful.mouse.client.resize(c)
    end
  )
)

awful.util.taglist_buttons =
  gears.table.join(
  awful.button(
    {},
    1,
    function(t)
      t:view_only()
    end
  ),
  awful.button({}, 3, awful.tag.viewtoggle)
)

clientbuttons_jetbrains =
  gears.table.join(
  awful.button({modkey}, 1, awful.mouse.client.move),
  awful.button({modkey}, 3, awful.mouse.client.resize)
)
-- }}

root.keys(keys.globalkeys)

return keys
