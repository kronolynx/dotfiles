local awful              = require("awful")
local utils              = require("utils")

terminal                 = os.getenv("TERMINAL") or "urxvt"
browser1                 = os.getenv("BROWSER") or "firefox"
browser2                 = "chrome"
editor                   = os.getenv("EDITOR") or "emacsclient -t"
editor_cmd               = terminal .. " -e " .. editor

file1                    = "thunar"
file2                    = terminal .. " -e " .. "ranger"
music                    = terminal .. " -e ncmpcpp"
my_screen_capture_window = "$HOME/.scripts/screen_shot.sh window"
my_screen_capture_area   = "$HOME/.scripts/screen_shot.sh area"
my_screen_capture_root   = "$HOME/.scripts/screen_shot.sh root"
logmenu                  = "$HOME/.scripts/logmenu.sh"
lockscreen               = "$HOME/.scripts/i3lock.sh lock"
my_launcher              = "rofi -show drun -no-plugins"

awful.util.terminal      = terminal

awful.layout.layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.fair,
  awful.layout.suit.spiral,
  awful.layout.suit.floating,
  -- awful.layout.suit.spiral.dwindle,
  --awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  -- awful.layout.suit.tile.top,
  awful.layout.suit.fair.horizontal,
  awful.layout.suit.max,
  -- awful.layout.suit.max.fullscreen,
  awful.layout.suit.magnifier,
  -- awful.layout.suit.corner.nw,
  -- awful.layout.suit.corner.ne,
  -- awful.layout.suit.corner.sw,
  -- awful.layout.suit.corner.se,
  utils.centerwork,
}

awful.util.my_colors = { -- TODO find a better place to store colors
  background = "#1D1F28",
  foreground = "#FDFDFD",
  light      = {
    black   = "#282A36", -- color0
    red     = "#F37F97", -- color1
    green   = "#5ADECD", -- color2
    yellow  = "#F2A272", -- color3
    blue    = "#8897F4", -- color4
    magenta = "#C574DD", -- color5
    cyan    = "#79E6F3", -- color6
    white   = "#FDFDFD", -- color7
    gray    = "#C0C0C0"
  },
  dark       = {
    black   = "#414458", -- color8
    red     = "#FF4971", -- color9
    green   = "#18E3C8", -- color10
    yellow  = "#FF8037", -- color11
    blue    = "#556FFF", -- color12
    magenta = "#B043D1", -- color13
    cyan    = "#3FDCEE", -- color14
    white   = "#BEBEC1", -- color15
    gray    = "#848482"
  }
}

awful.util.tagnames  = {
  {
    { name = "", sel = true },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "", lay = awful.layout.layouts[2], mw = 0.87 },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" }
  },
  {
    { name = "", sel = true },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" },
    { name = "" }
  }
}

local themes         = {
  "default" -- 1
}

chosen_theme         = themes[1]
