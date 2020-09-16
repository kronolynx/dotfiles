local awful = require("awful")
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()


-- ===================================================================
-- User variables and preferences
local rofi = "$HOME/.config/rofi/"
local scripts = "$HOME/.scripts/"
local terminal = os.getenv("TERMINAL") or "alacritty"
user = {
  terminal = terminal,
  floating_terminal = nerminal,
  browser = os.getenv("BROWSER") or "firefox",
  editor = "nvim",
  file1 = "thunar",
  file2 = terminal .. " -e " .. "ranger",
  music = terminal .. " -e ncmpcpp",
  screen_capture_window = scripts .."/screen_shot.sh window",
  screen_capture_area = scripts .."/screen_shot.sh area",
  screen_capture_root = scripts .."/screen_shot.sh root",
  logmenu = scripts .."/logmenu.sh",
  lockscreen = scripts .."/i3lock.sh lock",
  power_menu = rofi .. "bin/powermenu.sh",
  volume_menu = rofi .. "bin/volume.sh",
  battery_menu = rofi .. "bin/menu_battery.sh",
  launcher =  rofi .. "launchers/launcher.sh",
  window_selector = rofi .. "launchers/window.sh"
}

screen_width = awful.screen.focused().geometry.width
screen_height = awful.screen.focused().geometry.height

layouts = {
  tile = awful.layout.suit.tile,
  fairv = awful.layout.suit.fair,
  spiral = awful.layout.suit.spiral,
  floating = awful.layout.suit.floating,
  dwindle = awful.layout.suit.spiral.dwindle,
  left = awful.layout.suit.tile.left,
  bottom = awful.layout.suit.tile.bottom,
  top = awful.layout.suit.tile.top,
  horizontal = awful.layout.suit.fair.horizontal,
  max = awful.layout.suit.max,
  fullscreen = awful.layout.suit.max.fullscreen,
  magnifier = awful.layout.suit.magnifier,
  nw = awful.layout.suit.corner.nw,
  ne = awful.layout.suit.corner.ne,
  sw = awful.layout.suit.corner.sw,
  se = awful.layout.suit.corner.se,
}

awful.layout.layouts = {
  layouts.tile,
  layouts.fairv,
  layouts.floating,
  layouts.max,
}


-- Make dpi function global
dpi = beautiful.xresources.apply_dpi
-- Make xresources colors global
x = {
    --           xrdb variable
    background = xrdb.background,
    foreground = xrdb.foreground,
    color0     = xrdb.color0,
    color1     = xrdb.color1,
    color2     = xrdb.color2,
    color3     = xrdb.color3,
    color4     = xrdb.color4,
    color5     = xrdb.color5,
    color6     = xrdb.color6,
    color7     = xrdb.color7,
    color8     = xrdb.color8,
    color9     = xrdb.color9,
    color10    = xrdb.color10,
    color11    = xrdb.color11,
    color12    = xrdb.color12,
    color13    = xrdb.color13,
    color14    = xrdb.color14,
    color15    = xrdb.color15,
}


local themes = {
  "lena", -- 1
  "nord"
}
theme = themes[2]

-- ===================================================================
-- Statusbar themes. Multiple bars can be declared in each theme.
local bar_themes = {
  "ray", -- 1
  "noac"
}
bar_theme = bar_themes[2]

-- ===================================================================
local exit_screen_themes = {
    "lovelace",      -- 1 -- Uses image icons
    "ephemeral",     -- 2 -- Uses text-generated icons (consumes less RAM)
    "ray",           -- 3 -- WIP 
}
exit_screen_theme = exit_screen_themes[1]

-- ===================================================================
-- Affects which icon theme will be used by widgets that display image icons.
local icon_themes = {
    "linebit",        -- 1 -- Neon + outline
    "drops",          -- 2 -- Pastel + filled
}
icon_theme = icon_themes[2]
-- ===================================================================