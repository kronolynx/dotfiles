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

local update_widget = function(widget, stdout)
  local escape_f  = require("awful.util").escape
  local text = ""
  local artist = ""
  local title = ""
  local state_icon = string.match(stdout, "Playing") and '' or
      (string.match(stdout, "Paused") and  '' or nil)

  if state_icon then
    for k, v in string.gmatch(stdout, "'[^:]+:([^']+)':[%s]<%[?'([^']+)'%]?>")
    do
      if k == "artist" then artist = escape_f(v)
      elseif k == "title" then title = escape_f(v)
      end
    end

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

