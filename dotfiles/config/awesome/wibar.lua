-- Important !! This should be imported after the theme has been loaded (beautiful.init)

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox     = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
local widgets   = require("widgets")

local wibar     = {}

wibar.setup     = function(s)
  -- Tags
  for _, i in pairs(awful.util.tagnames[s.index]) do
    awful.tag.add(i.name, {
      layout              = i.lay or awful.layout.layouts[1],
      gap                 = i.gap or beautiful.useless_gap,
      gap_single_client   = true,
      screen              = s,
      selected            = i.sel or false,
      master_width_factor = i.mw or 0.5,
    })
  end

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()
  -- Create an imagebox widget which will contains an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)
  s.mylayoutbox:buttons(
      gears.table.join(
          awful.button({}, 2, function() awful.layout.set(awful.layout.layouts[1]) end)
      )
  )

  -- Create a taglist widget
  s.mytaglist            = awful.widget.taglist {
    screen  = s,
    -- filter  = awful.widget.taglist.filter.all,
    filter  = function (t) return t.selected or #t:clients() > 0 end,
    buttons = awful.util.taglist_buttons
  }

  local tasklist_buttons = gears.table.join(
      awful.button({ }, 3, function()
        awful.menu.client_list({ theme = { width = 250 } })
      end)
  )

  -- Create a tasklist widget
  s.mytasklist      = awful.widget.tasklist {
    screen          = s,
    filter          = awful.widget.tasklist.filter.focused,
    buttons         = tasklist_buttons,
    style           = {
      shape_border_width = 1,
      shape_border_color = '#777777',
      shape              = gears.shape.rounded_bar,
    },
    layout          = {
      spacing = 3,
      layout  = wibox.layout.flex.horizontal
    },
    widget_template = {
      {
        {
          {
            {
              id     = 'icon_role',
              widget = wibox.widget.imagebox,
            },
            margins = 2,
            widget  = wibox.container.margin,
          },
          {
            id     = 'text_role',
            widget = wibox.widget.textbox,
          },
          layout = wibox.layout.fixed.horizontal,
        },
        left   = 10,
        right  = 10,
        widget = wibox.container.margin
      },
      id           = 'background_role',
      forced_width = 230,
      widget       = wibox.container.background,
    }
  }

  -- Create the wibox
  s.mywibox              = awful.wibar({ position = "top", screen = s, height = 26 })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    expand = 'none',
    { -- Left widgets
      layout = wibox.layout.fixed.horizontal,
      s.mytaglist,
      widgets.space,
      s.mypromptbox,
      s.mytasklist,
      widgets.space,
    },
    widgets.mpris,
    { -- Right widgets
      layout = wibox.layout.fixed.horizontal,
      id     = "rightwidgets",
      widgets.kblayout,
      widgets.vol,
      widgets.textclock,
      wibox.widget.systray(),
      s.mylayoutbox
    },
  }
  -- if laptop then add battery widget
  awful.spawn.easy_async_with_shell("acpi -b", function(c)
    if c:find("Battery") then s.mywibox:get_children_by_id("rightwidgets")[1]:insert(3, widgets.bat) end
  end)
end

return wibar
