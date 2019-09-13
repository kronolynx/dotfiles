local awful = require("awful")
local wibox = require("wibox")
local build_widget = require("widgets.build_widget")


local kblayout_text = wibox.widget{
  markup = "",
  widget = wibox.widget.textbox,
}

local function update_layout()
  local layouts_raw = awful.widget.keyboardlayout.get_groups_from_group_names(
    awesome.xkb_get_group_names())
  local cur_layout = awesome.xkb_get_layout_group()
  if layouts_raw == nil or layouts_raw[1] == nil then
      gdebug.print_error("Failed to get list of keyboard groups")
      return
  end
  local layouts = {}
  for _, v in ipairs(layouts_raw) do
    layouts[v.group_idx] = v.section and string.sub(v.section, 1,2) or v.file
  end
  kblayout_text.markup = "<span foreground='#cc00ff'>" .. layouts[cur_layout+1]:upper() .. "</span>"
end

awesome.connect_signal("xkb::map_changed",
  function() update_layout()
end)

awesome.connect_signal("xkb::group_changed",
  function() update_layout()
end)

update_layout()

keyboard = build_widget:new(kblayout_text, " ", "darksalmon", " ")

return keyboard.widget
