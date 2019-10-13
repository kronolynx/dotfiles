local awful          = require("awful")
local wibox          = require("wibox")

local vol_icons      = {
  mute = "婢",
  vlow = "",
  low  = "奔",
  mid  = "墳",
  high = ""
}

local get_vol_status = function(c)
  local status
  local percentage
  for line in string.gmatch(c, "[^\r\n]+") do
    if not status and not percentage then
      percentage, status = string.match(line, "%[(%d+).%]%s%[(%a+)%]")
    end
  end
  return status, tonumber(percentage)
end

local vol            = awful.widget.watch(
    { awful.util.shell, "-c", "amixer -D pulse get Master" },
    2,
    function(widget, stdout)
      local status, cur_vol = get_vol_status(stdout)
      local current_icon
      local current_vol

      if status == "on" then
        local vol_icon
        if cur_vol >= 70 then
          vol_icon = vol_icons.high
        elseif cur_vol >= 40 then
          vol_icon = vol_icons.mid
        elseif cur_vol >= 15 then
          vol_icon = vol_icons.low
        else
          vol_icon = vol_icons.vlow
        end
        current_icon = "<span foreground='" .. awful.util.my_colors.dark.green .. "'>" .. vol_icon .. "</span>"
        current_vol  = cur_vol .. " "
      else
        current_icon = "<span foreground='" .. awful.util.my_colors.dark.gray .. "'><s>" .. vol_icons.mute .. "</s></span>"
        current_vol  = "<span foreground='" .. awful.util.my_colors.dark.gray .. "'><s>" .. cur_vol .. "</s></span> "
      end
      widget:set_markup(current_icon, current_vol)
    end,
    wibox.widget {
      {
        id     = 'current_icon',
        widget = wibox.widget.textbox,
      },
      {
        id     = 'current_vol',
        widget = wibox.widget.textbox,
      },
      layout     = wibox.layout.fixed.horizontal,
      spacing    = 5,
      set_markup = function(self, icon, percentage)
        self.current_icon.markup = icon
        self.current_vol.markup  = percentage
      end,
    }
)

vol:buttons(awful.util.table.join(
    awful.button({}, 2, function()
      -- left click
      awful.spawn("pavucontrol")
    end),
    awful.button({}, 3, function()
      -- right click
      awful.spawn.with_shell("pulsemixer --toggle-mute")
    end),
    awful.button({}, 4, function()
      -- scroll up
      awful.spawn.with_shell("pulsemixer --change-volume +2")
    end),
    awful.button({}, 5, function()
      -- scroll down
      awful.spawn.with_shell("pulsemixer --change-volume -2")
    end)
))

return vol

