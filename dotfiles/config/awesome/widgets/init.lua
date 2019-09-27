local awful = require("awful")
local wibox = require("wibox")
local build_widget = require("widgets.build_widget")
local evil = require("evil")

local widgets = {}

-- From Files
widgets.vol = require('widgets.vol')
widgets.bat = require('widgets.bat')
widgets.mpris = require('widgets.mpris')
widgets.kblayout = require('widgets.kblayout')

-- Separators
widgets.space = wibox.widget.textbox('<span>  </span>')
widgets.seperator = wibox.widget.textbox(' <span color="grey"> </span>  ')

-- Textclock
local textclock_text =wibox.widget.textclock(" %d %b %a %I:%M %p")
widgets.textclock = build_widget:new(textclock_text, "", "#a753fc", true).widget

local month_calendar = awful.widget.calendar_popup.month()
month_calendar:attach( widgets.textclock, "tc" )

return widgets
