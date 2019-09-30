local awful         = require("awful")
local wibox         = require("wibox")

local mpris_widget         = wibox.widget {
  {
    id     = 'current_song',
    widget = wibox.widget.textbox,
  },
  layout     = wibox.layout.align.horizontal,
  set_text   = function(self, path)
    self.current_song.markup = path
  end,
}

local function get_playing_now(c)
   local artist = nil
   local title = nil
   for line in string.gmatch(c, "[^\r\n]+") do
     if not artist then
       artist = string.match(line, ".*artist%s*(.+)")
     end
     if not title then
       title = string.match(line, ".*title%s*(.+)")
     end
   end
   return artist, title
 end

local update_widget = function(widget, stdout)
  local escape_f  = require("awful.util").escape
  local text = ""
  local state_icon = string.match(stdout, "Playing") and '' or
      (string.match(stdout, "Paused") and  '' or nil)

  if state_icon then
    local artist, title = get_playing_now(stdout)

    text = state_icon .. " " .. artist .. " - " .. title
  end

  widget:set_text(text)
end

awful.widget.watch(
    { awful.util.shell, "-c", "playerctl status && playerctl metadata" },
    2,
    update_widget,
    mpris_widget
)

return mpris_widget

