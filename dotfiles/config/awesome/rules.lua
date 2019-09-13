local awful       = require("awful")
local gears       = require("gears")
local beautiful   = require("beautiful")

awful.rules.rules = {
  -- All clients will match this rule.
  { rule       = { },
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
    rule = {
        class = "jetbrains-.*",
    }, properties = { focus = true, buttons = clientbuttons_jetbrains }
  },
  {
    rule = {
        class = "jetbrains-.*",
        name = "win.*"
    }, properties = { titlebars_enabled = false, focusable = false, focus = true, floating = true, placement = awful.placement.restore }
  },

  -- Floating clients.
  { rule_any = {
      instance = {
        "DTA",  -- Firefox addon DownThemAll.
        "copyq",  -- Includes session name in class.
        "pinentry",
      },
      class = {
        "Arandr",
        "Blueman-manager",
        "Gpick",
        "Kruler",
        "MessageWin",  -- kalarm.
        "Sxiv",
        "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
        "Wpa_gui",
        "veromix",
        "xtightvncviewer"},

      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name = {
        "Event Tester",  -- xev.
      },
      role = {
        "AlarmWindow",  -- Thunderbird's calendar.
        "ConfigManager",  -- Thunderbird's about:config.
        "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
      }
    }, properties = { floating = true }
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
  { rule = { class = "jetbrains-*" },
    properties = {
      floating = false,
      type = "normal"
    } }

}
