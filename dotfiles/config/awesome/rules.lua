local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

-- Get screen geometry
local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

-- Rules
-- ===================================================================
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  {
    rule = {},
    properties = {
      border_width = beautiful.border_width,
      border_color = beautiful.border_normal,
      focus = awful.client.focus.filter,
      raise = true,
      keys = keys.clientkeys,
      buttons = keys.clientbuttons,
      -- screen           = awful.screen.preferred,
      screen = awful.screen.focused,
      size_hints_honor = true,
      honor_workarea = true,
      honor_padding = true,
      maximized = false,
      titlebars_enabled = beautiful.titlebars_enabled,
      maximized_horizontal = false,
      maximized_vertical = false,
      placement = awful.placement.centered + awful.placement.no_overlap + awful.placement.no_offscreen
    }
  },
  -- Floating clients
  {
    rule_any = {
      instance = {
        "DTA", -- Firefox addon DownThemAll.
        "copyq", -- Includes session name in class.
        "floating_terminal",
        "riotclientux.exe",
        "leagueclientux.exe",
        "Devtools" -- Firefox devtools
      },
      class = {
        "Gpick",
        "Lxappearance",
        "Nm-connection-editor",
        "File-roller",
        "fst",
        "Nvidia-settings"
      },
      name = {
        "Event Tester", -- xev
        "MetaMask Notification"
      },
      role = {
        "AlarmWindow",
        "pop-up",
        "GtkFileChooserDialog",
        "conversation"
      },
      type = {
        "dialog"
      }
    },
    properties = {floating = true}
  },
  -- Centered clients
  {
    rule_any = {
      type = {
        "dialog"
      },
      class = {
        "Steam",
        "discord",
        "music",
        "scratchpad"
      },
      instance = {
        "music",
        "scratchpad"
      },
      role = {
        "GtkFileChooserDialog",
        "conversation"
      }
    },
    properties = {placement = centered_client_placement}
  },
  -- Titlebars ON (explicitly)
  {
    rule_any = {
      type = {
        "dialog"
      },
      role = {
        "conversation"
      }
    },
    callback = function(c)
      decorations.show(c)
    end
  },
  -- Fixed terminal geometry for floating terminals
  {
    rule_any = {
      class = {
        "Alacritty",
        "Termite",
        "mpvtube",
        "kitty",
        "st-256color",
        "st",
        "URxvt"
      }
    },
    properties = {width = screen_width * 0.45, height = screen_height * 0.5}
  },
  -- Pavucontrol
  {
    rule_any = {class = {"Pavucontrol"}},
    properties = {floating = true, width = screen_width * 0.45, height = screen_height * 0.8}
  },
  -- File managers
  {
    rule_any = {
      class = {
        "Nemo",
        "Thunar"
      }
    },
    except_any = {
      type = {"dialog"}
    },
    properties = {floating = true, width = screen_width * 0.70, height = screen_height * 0.80}
  },
  ---------------------------------------------
  -- Start application on specific workspace --
  ---------------------------------------------
  -- Browsing
  {
    rule_any = {
      class = {
        "firefox"
      }
    },
    except_any = {
      role = {"GtkFileChooserDialog"},
      instance = {"Toolkit"},
      type = {"dialog"}
    },
    properties = {screen = 1, tag = awful.screen.focused().tags[1]}
  },
  -- intellij
  {
    rule_any = {
      class = {
        "jetbrains-idea-ce"
      }
    },
    except_any = {
      role = {"GtkFileChooserDialog"},
      instance = {"Toolkit"},
      type = {"dialog"}
    },
    properties = {screen = 1, tag = awful.screen.focused().tags[3]},
    callback = function(c)
      decorations.show(c)
    end
  },
  -- Chatting
  {
    rule_any = {
      class = {
        "discord",
        -- "TelegramDesktop",
        "Signal",
        "Slack",
        "zoom",
        "weechat"
      }
    },
    except_any = {
      type = {"dialog"}
    },
    properties = {screen = 1, tag = awful.screen.focused().tags[10]}
  },
  -- Miscellaneous
  -- All clients that I want out of my way when they are running
  {
    rule_any = {
      class = {
        "torrent",
        "Transmission",
        "Deluge",
        "VirtualBox Manager",
        "KeePassXC"
      },
      instance = {
        "torrent",
        "qemu"
      }
    },
    except_any = {
      type = {"dialog"}
    },
    properties = {screen = 1, tag = awful.screen.focused().tags[12]}
  }
}
