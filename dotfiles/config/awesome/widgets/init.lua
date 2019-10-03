local wibox       = require("wibox")

local widgets     = {}

-- From Files
widgets.vol       = require('widgets.vol')
widgets.bat       = require('widgets.bat')
widgets.mpris     = require('widgets.mpris')
widgets.kblayout  = require('widgets.kblayout')
widgets.textclock = require('widgets.clock')

-- Separators
widgets.space     = wibox.widget.textbox('<span>  </span>')
widgets.separator = wibox.widget.textbox(' <span color="grey"> </span>  ')

return widgets
