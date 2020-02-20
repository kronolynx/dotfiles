local awful = require("awful")
local wibox = require("wibox")

local kb    = wibox.widget {
  {
    markup = '<span color="' .. awful.util.my_colors.dark.magenta .. '"></span>',
    widget = wibox.widget.textbox,
    forced_width = 22
  },
  {
    id     = "cur_layout",
    widget = wibox.widget.textbox,
  },
  spacing    = 0,
  layout     = wibox.layout.fixed.horizontal,
  set_markup = function(self, layout)
    self.cur_layout.markup = layout
  end
}

local function update_layout(widget)
  local layouts_raw = awful.widget.keyboardlayout.get_groups_from_group_names(
      awesome.xkb_get_group_names())
  local cur_layout  = awesome.xkb_get_layout_group()
  if layouts_raw == nil or layouts_raw[1] == nil then
    gdebug.print_error("Failed to get list of keyboard groups")
    return
  end
  local layouts = {}
  for _, v in ipairs(layouts_raw) do
    layouts[v.group_idx] = v.section and string.sub(v.section, 1, 2) or v.file
  end
  widget:set_markup("<span foreground='#cc00ff'>" .. layouts[cur_layout + 1]:upper() .. "</span> ")
end

awesome.connect_signal("xkb::map_changed", function() update_layout(kb) end)

awesome.connect_signal("xkb::group_changed", function() update_layout(kb) end)

update_layout(kb)

return kb
