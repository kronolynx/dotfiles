local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")

-- Helper function that updates a taglist item
local update_taglist = function(item, tag, index)
  if tag.selected then
    item.markup = helpers.colorize_text(beautiful.taglist_text_focused[index], beautiful.taglist_fg_focus)
  elseif tag.urgent then
    item.markup = helpers.colorize_text(beautiful.taglist_text_urgent[index], beautiful.taglist_fg_urgent)
  elseif #tag:clients() > 0 then
    item.markup = helpers.colorize_text(beautiful.taglist_text_occupied[index], beautiful.taglist_fg_occupied)
  else
    item.markup = helpers.colorize_text(beautiful.taglist_text_empty[index], beautiful.taglist_fg_empty)
  end
end

-- {{{ Wibar
-- Create a textclock widget
local mytextclock = wibox.widget.textclock(helpers.colorize_text("%d %b %a %H:%M", beautiful.clocktext_fg))

awful.screen.connect_for_each_screen(
  function(s)
    -- Each screen has its own tag table.
    awful.tag({"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"}, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)

    -- Create a taglist widget
    s.mytaglist =
      awful.widget.taglist {
      screen = s,
      filter = awful.widget.taglist.filter.all,
      widget_template = {
        widget = wibox.widget.textbox,
        create_callback = function(self, tag, index, _)
          self.align = "left"
          self.valign = "center"
          self.font = beautiful.taglist_text_font
          update_taglist(self, tag, index)
        end,
        update_callback = function(self, tag, index, _)
          update_taglist(self, tag, index)
        end
      },
      buttons = awful.util.taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist =
      awful.widget.tasklist {
      screen = s,
      filter = awful.widget.tasklist.filter.focused,
      widget_template = {
        -- {
        --  wibox.widget.base.make_widget(),
        --  forced_height = 5,
        --  id            = 'background_role',
        --  widget        = wibox.container.background,
        --},
        {
          {
            id = "clienticon",
            widget = awful.widget.clienticon
          },
          margins = 5,
          widget = wibox.container.margin
        },
        nil,
        layout = wibox.layout.align.vertical
      }
      -- buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({position = "top", screen = s, bg = beautiful.bg_normal})

    s.systray = wibox.widget.systray()
    -- s.systray:set_base_size(21)
    s.systray.visible = false

    -- s.focused_window = ""

    -- Add widgets to the wibox
    s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      expand = "none",
      {
        -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        s.mytaglist,
        s.mypromptbox
      },
      mytextclock,
       --s.mytasklist, -- Middle widget
      {
        -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        wibox.layout.margin(s.systray, 1, 1, 4, 2),
        s.mytasklist,
        s.mylayoutbox
      }
    }
  end
)
-- }}}
