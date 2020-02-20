local awful          = require("awful")
local wibox          = require("wibox")

local textclock      = wibox.widget {
  {
    markup = ' <span color="' .. awful.util.my_colors.dark.magenta .. '"></span>',
    widget = wibox.widget.textbox,
    forced_width = 23
  },
  {
    markdown = "%d %b %a %H:%M",
    widget   = wibox.widget.textclock
  },
  spacing = 2,
  layout  = wibox.layout.fixed.horizontal
}

local month_calendar = awful.widget.calendar_popup.month()
month_calendar:attach(textclock, "tr")

return textclock
