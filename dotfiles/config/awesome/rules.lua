local awful       = require("awful")
local beautiful   = require("beautiful")
local gears  = require("gears")

-- Get screen geometry
local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

awful.rules.rules = {
  -- All clients will match this rule.
  {
    rule       = { },
    properties = { border_width     = beautiful.border_width,
                   border_color     = beautiful.border_normal,
                   focus            = awful.client.focus.filter,
                   keys             = clientkeys,
                   raise            = false,
                   buttons          = clientbuttons,
                   screen           = awful.screen.preferred,
                   size_hints_honor = false,
    }
  },
  {
    rule          = {
      class = "jetbrains-.*",
    }, properties = { focus = true, buttons = clientbuttons_jetbrains }
  },
  {
    rule          = {
      class = "jetbrains-.*",
      name  = "win.*"
    }, properties = { titlebars_enabled = false, focusable = false, focus = true, floating = true, placement = awful.placement.restore }
  },

  -- Floating clients.
  {
    rule_any      = {
      instance = {
        "DTA", -- Firefox addon DownThemAll.
        "copyq", -- Includes session name in class.
        "pinentry",
      },
      class    = {
        "Arandr",
        "Blueman-manager",
        "Gpick",
        "Kruler",
        "MessageWin", -- kalarm.
        "Sxiv",
        "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
        "Wpa_gui",
        "veromix",
        "xtightvncviewer" },

      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name     = {
        "Event Tester", -- xev.
      },
      role     = {
        "AlarmWindow", -- Thunderbird's calendar.
        "ConfigManager", -- Thunderbird's about:config.
        "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
      }
    }, properties = { floating = true },
    callback = function (c)
      awful.placement.centered(c,nil)
    end
  },

  -- Steam Rules
  { rule = { class = "Steam" }, properties = { screen = 1, tag = "9", } },

  {
    rule       = { class = "Steam" },
    except     = { name = "Steam" },
    properties = { ontop = true, }
  },

  -- Keep dialogs on top
  {
    rule_any   = {
      class = { "file_progress", },
      type  = { "dialog" },
    },
    properties = { ontop = true, },
  },
  { rule       = { class = "jetbrains-*" },
    properties = {
      floating = false,
      type     = "normal"
    }
  },

  -- Fixed terminal geometry
  {
    rule_any   = {
      class = {
        "Alacritty",
        "Termite",
        "mpvtube",
        "kitty",
        "st-256color",
        "st",
        "URxvt",
      },
    },
    properties = { width = screen_width * 0.45, height = screen_height * 0.5 }
  },

  -- File chooser dialog
  {
    rule_any   = { role = { "GtkFileChooserDialog" } },
    properties = { floating = true, width = screen_width * 0.55, height = screen_height * 0.65 }
  },

  -- Pavucontrol
  {
    rule_any   = { class = { "Pavucontrol" } },
    properties = { floating = true, width = screen_width * 0.45, height = screen_height * 0.8 }
  },
  -- File managers
  {
    rule_any   = {
      class = {
        "Nemo",
        "Thunar"
      },
    },
    except_any = {
      type = { "dialog" }
    },
    properties = { floating = false, width = screen_width * 0.45, height = screen_height * 0.55 }
  },
  -- Scratchpad
  {
    rule_any   = {
      instance  = { "scratchpad" },
      class     = { "scratchpad" },
      icon_name = { "scratchpad_urxvt" },
    },
    properties = {
      skip_taskbar = false,
      floating     = true,
      ontop        = false,
      minimized    = true,
      sticky       = false,
      width        = screen_width * 0.7,
      height       = screen_height * 0.75
    },
    callback   = function(c)
      awful.placement.centered(c, { honor_padding = true, honor_workarea = true })
      gears.timer.delayed_call(function()
        c.urgent = false
      end)
    end
  },
  -- Chatting
  {
    rule_any   = {
      class = {
        "discord",
        "TelegramDesktop",
        "Signal",
        "Slack",
      },
    },
    properties = { screen = function () return screen.count() >= 2 and screen[2] or screen[1] end, tag = awful.screen.focused().tags[10], floating = false }
  },
  {
    rule_any   = {
      class = {
        "Spotify",
      },
    },
    properties = { screen = function () return screen.count() >= 2 and screen[2] or screen[1] end, tag = awful.screen.focused().tags[10], floating = false }
  }
}
