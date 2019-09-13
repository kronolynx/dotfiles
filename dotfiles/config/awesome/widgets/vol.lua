local awful          = require("awful")
local wibox          = require("wibox")
local build_widget   = require("widgets.build_widget")

local vol_perc       = wibox.widget {
  markup = "00%",
  align  = 'center',
  valign = 'center',
  widget = wibox.widget.textbox
}
local vol_icons      = {
  mute = "婢",
  vlow = "",
  low  = "奔",
  mid  = "墳",
  high = ""
}
local vol_icon       = vol_icons.high
local vol_icon_color = "#ff8e1e" -- TODO put colors in to variables

awesome.connect_signal("evil::volume", function(volume, muted)
  local cur_vol   = tonumber(volume)
  vol_perc.markup = string.format("%02d%%", cur_vol)

  if cur_vol >= 70 then
    vol_icon = vol_icons.high
  elseif cur_vol >= 40 then
    vol_icon = vol_icons.mid
  elseif cur_vol >= 15 then
    vol_icon = vol_icons.low
  else
    vol_icon = vol_icons.vlow
  end

  if muted then
    vol_icon_color  = "gray"
    vol_icon        = vol_icons.mute
    vol_perc.markup = "<span foreground='grey'><s>" .. vol_perc.markup .. "</s></span>"
  else
    vol_icon_color = "#ff8e1e"
  end
  vol:UpdateIcon(vol_icon, vol_icon_color)
end
)

vol = build_widget:new(vol_perc, vol_icon, vol_icon_color, true)

vol.widget:buttons(awful.util.table.join(
    awful.button({}, 2, function()
      -- left click
      awful.spawn("pavucontrol")
    end),
    awful.button({}, 3, function()
      -- right click
      os.execute("pulsemixer --toggle-mute")
    end),
    awful.button({}, 4, function()
      -- scroll up
      os.execute("pulsemixer --change-volume +2")
    end),
    awful.button({}, 5, function()
      -- scroll down
      os.execute("pulsemixer --change-volume -2")
    end)
))

return vol.widget
