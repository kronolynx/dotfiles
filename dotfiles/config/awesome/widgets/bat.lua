local awful        = require("awful")
local wibox        = require("wibox")
local beautiful    = require("beautiful")
local build_widget = require("widgets.build_widget")

local bat_icons    = {
  empty   = "",
  di10    = "",
  di20    = "",
  di30    = "",
  di40    = "",
  di50    = "",
  di60    = "",
  di70    = "",
  di80    = "",
  di90    = "",
  difull  = "",
  chempty = "",
  ch20    = "",
  ch50    = "",
  ch60    = "",
  ch80    = "",
  ch90    = "",
  chfull  = ""
}
local bat_colors   = { -- TODO update colors
  full       = "green",
  charged    = "blue",
  discharged = "orange",
  warn       = "red"
}

local bat_perc       = wibox.widget { -- TODO replace this with an icon widget
  markup = "",
  align  = 'center',
  valign = 'center',
  widget = wibox.widget.textbox
}

local bat_icon       = ""
local is_plugged     = true

local bat_icon_color = bat_colors.full

awesome.connect_signal("evil::charger", function(plugged)
  is_plugged = plugged
end)

awesome.connect_signal("evil::battery", function(value)
  local bat_now = value or 0

  if is_plugged then
    if bat_now >= 100 then
      bat_icon = bat_icons.chfull
    elseif bat_now >= 90 then
      bat_icon = bat_icons.ch90
    elseif bat_now >= 80 then
      bat_icon = bat_icons.ch80
    elseif bat_now >= 60 then
      bat_icon = bat_icons.ch60
    elseif bat_now >= 40 then
      bat_icon = bat_icons.ch50
    elseif bat_now >= 20 then
      bat_icon = bat_icons.ch20
    else
      bat_icon = bat_icons.empty
    end
  else
    if bat_now >= 100 then
      bat_icon = bat_icons.difull
    elseif bat_now >= 90 then
      bat_icon = bat_icons.di90
    elseif bat_now >= 80 then
      bat_icon = bat_icons.di80
    elseif bat_now >= 70 then
      bat_icon = bat_icons.di70
    elseif bat_now >= 60 then
      bat_icon = bat_icons.di60
    elseif bat_now >= 50 then
      bat_icon = bat_icons.di50
    elseif bat_now >= 40 then
      bat_icon = bat_icons.di40
    elseif bat_now >= 30 then
      bat_icon = bat_icons.di30
    elseif bat_now >= 20 then
      bat_icon = bat_icons.di20
    elseif bat_now >= 10 then
      bat_icon = bat_icons.di10
    else
      bat_icon = bat_icons.empty
    end
  end

  if bat_now >= 90 then
    bat_icon_color = bat_colors.full
  elseif bat_now >= 45 then
    bat_icon_color = bat_colors.charged
  elseif bat_now >= 20 then
    bat_icon_color = bat_colors.discharged
  else
    bat_icon_color = bat_colors.warn
  end
  bat:UpdateIcon(bat_icon, bat_icon_color)
end)

bat = build_widget:new(bat_perc, bat_icon, bat_icon_color, true)

bat.widget:buttons(awful.util.table.join(
    awful.button({}, 4, function()
      -- scroll up
      awful.spawn.with_shell("xbacklight + 5 -time 100 -steps 1")
    end),
    awful.button({}, 5, function()
      -- scroll down
      awful.spawn.with_shell("xbacklight - 6 -time 100 -steps 1")
    end)
))
return bat.widget
