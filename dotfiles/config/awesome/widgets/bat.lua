local awful          = require("awful")
local wibox          = require("wibox")

local bat_icons      = {
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

local bat_colors     = { -- TODO update colors
  full       = "lime",
  charged    = "blue",
  discharged = "orange",
  warn       = "red"
}

local build_icon     = function(icon, icon_color)
  return '<span color="' .. icon_color .. '">' .. icon .. '</span>'
end

local get_bat_status = function(c)
  local status, percentage = string.match(c, ":%s(%a+),%s(%d+)")
  local is_charging        = status and status ~= 'Discharging'

  return is_charging, (tonumber(percentage) or 0)
end

local bat            = awful.widget.watch(
    { awful.util.shell, "-c", "acpi -b" },
    20,
    function(widget, stdout)
      local is_plugged, bat_now = get_bat_status(stdout)
      local bat_icon
      local bat_icon_color

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
          bat_icon = bat_icons.chempty
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

      widget:set_markup(build_icon(bat_icon, bat_icon_color))
    end,
    wibox.widget {
      {
        id     = 'current_bat',
        widget = wibox.widget.textbox,
      },
      layout     = wibox.layout.align.horizontal,
      spacing    = 5,
      set_markup = function(self, icon)
        self.current_bat.markup = icon
      end,
    }
)

bat:buttons(awful.util.table.join(
    awful.button({}, 4, function()
      -- scroll up
      awful.spawn.with_shell("xbacklight + 5 -time 100 -steps 1")
    end),
    awful.button({}, 5, function()
      -- scroll down
      awful.spawn.with_shell("xbacklight - 6 -time 100 -steps 1")
    end)
))

return bat

