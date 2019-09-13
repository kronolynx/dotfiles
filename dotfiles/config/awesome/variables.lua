local awful = require("awful")
local utils = require("utils")

terminal = "urxvtc"
browser1 = "firefox"
browser2 = "chrome"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor
file1 = "thunar"
file2 = terminal .. " -e " .. "ranger"
music = terminal .. " -e ncmpcpp"
my_screen_capture = "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/'"
my_launcher = "rofi -show drun -no-plugins"
lockscreen = "$HOME/.scripts/i3lock.sh lock"
logmenu = "$HOME/.scripts/logmenu.sh"

-- Menu
myawesomemenu   = {
  { "hotkeys", function() return false, hotkeys_popup.show_help end },
  { "manual", terminal .. " -e man awesome" },
  { "edit config", string.format("%s -e %s %s", terminal, editor, awesome.conffile) },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end }
}

awful.util.terminal = terminal

awful.layout.layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.floating,
  awful.layout.suit.spiral,
  -- awful.layout.suit.spiral.dwindle,
  --awful.layout.suit.tile.left,
  awful.layout.suit.tile.bottom,
  -- awful.layout.suit.tile.top,
  awful.layout.suit.fair,
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

awful.util.tagnames = {
	{
		{name = "", sel = true},
		{name = ""},
		{name = ""}, 
		{name = ""},
		{name = ""},
		{name = ""},
		{name = ""},
		{name = "", lay = awful.layout.layouts[3], mw = 0.87},
		{name = ""}
	},
	{
		{name = "", sel = true},
		{name = ""},
		{name = ""}, 
		{name = ""},
		{name = ""},
		{name = ""},
		{name = ""},
		{name = ""},
		{name = ""}
	}
}

local themes = {
    "default"      -- 1
}

chosen_theme = themes[1]
