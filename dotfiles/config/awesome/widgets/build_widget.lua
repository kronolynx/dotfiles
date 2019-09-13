local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local build_widget = {}

local build_icon = function (icon, icon_color)
  return '<span color="' .. icon_color .. '" font="' .. beautiful.iconFont .. '">' .. icon .. '</span>'
end

function build_widget:new (value_widget, icon, icon_color, last)
  obj = {}
  value_widget.font = beautiful.font
  if last then
    obj.is_last = wibox.widget.textbox(' ')
  end
  obj.pipe = obj.is_last or wibox.widget.textbox('<span color="grey">|</span> ')
  
  obj.widget_icon = wibox.widget{
    markup = build_icon(icon, icon_color),
    align  = 'center',
    valign = 'center',
    widget = wibox.widget.textbox
  }

  obj.widget = wibox.widget{
    nil,
    {
      obj.widget_icon,
      value_widget,
      obj.pipe,
      spacing = dpi(3),
      layout = wibox.layout.fixed.horizontal
    },
    expand = "none",
    layout = wibox.layout.align.horizontal
  }

  self.__index = self  
  return setmetatable(obj, self)
end

function build_widget:UpdateIcon(icon, icon_color)
  self.widget_icon.markup = build_icon(icon, icon_color)
end

return build_widget
