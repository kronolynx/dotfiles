-- Important !! This should be imported after the theme has been loaded (beautiful.init)

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
local widgets = require("widgets")

local tagnames = {
  {
    {name = "一", sel = true},
    {name = "二", lay = awful.layout.layouts[2], mw = 0.87},
    {name = "三"},
    {name = "四"},
    {name = "五"},
    {name = "六"},
    {name = "七"},
    {name = "八"},
    {name = "九"},
    {name = "十", lay = awful.layout.layouts[3], mw = 0.87},
    {name = "零"},
    {name = "人"}
  },
  {
    {name = "一", sel = true},
    {name = "二"},
    {name = "三"},
    {name = "四"},
    {name = "五"},
    {name = "六"},
    {name = "七"},
    {name = "八"},
    {name = "九"},
    {name = "十"},
    {name = "零"},
    {name = "人"}
  }
}

awful.screen.connect_for_each_screen(
  function(s)
    -- Tags
    for _, i in pairs(tagnames[s.index]) do
      awful.tag.add(
        i.name,
        {
          layout = i.lay or awful.layout.layouts[1],
          gap = i.gap or beautiful.useless_gap,
          gap_single_client = true,
          screen = s,
          selected = i.sel or false,
          master_width_factor = i.mw or 0.5
        }
      )
    end

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(
      gears.table.join(
        awful.button(
          {},
          2,
          function()
            awful.layout.set(awful.layout.layouts[1])
          end
        )
      )
    )

    local function update_tag(self, t, index, objects)
      local col = t.selected and beautiful.border_focus or beautiful.bg_normal
      self:get_children_by_id("current_tag")[1].color = col
    end

    -- Create a taglist widget
    s.mytaglist =
      awful.widget.taglist {
      screen = s,
      filter = function(t)
        return t.selected or #t:clients() > 0
      end,
      buttons = awful.util.taglist_buttons,
      layout = {
        spacing = -12,
        spacing_widget = {
          color = beautiful.bg_normal,
          shape = gears.shape.parallelogram,
          widget = wibox.widget.separator
        },
        layout = wibox.layout.fixed.horizontal
      },
      widget_template = {
        {
          {
            {
              {
                {
                  id = "text_role",
                  widget = wibox.widget.textbox
                },
                layout = wibox.layout.fixed.horizontal
              },
              left = 2,
              right = 2,
              widget = wibox.container.margin
            },
            id = "background_role",
            widget = wibox.container.background
          },
          bottom = 2,
          color = beautiful.bg_normal,
          widget = wibox.container.margin,
          id = "current_tag"
        },
        left = 3,
        right = 3,
        layout = wibox.container.margin,
        create_callback = function(self, t, index, objects)
          local col = t.selected and beautiful.border_focus or beautiful.bg_normal
          local current_tag = self:get_children_by_id("current_tag")[1]

          current_tag.color = col

          t:connect_signal(
            "property::urgent",
            function()
              current_tag.color = beautiful.fg_urgent
            end
          )
        end,
        update_callback = function(self, t, index, objects)
          local col = t.selected and beautiful.border_focus or beautiful.bg_normal
          self:get_children_by_id("current_tag")[1].color = col
        end
      }
    }

    local tasklist_buttons =
      gears.table.join(
      awful.button(
        {},
        3,
        function()
          awful.menu.client_list({theme = {width = 250}})
        end
      )
    )

    -- Create a tasklist widget
    s.mytasklist =
      awful.widget.tasklist {
      screen = s,
      filter = awful.widget.tasklist.filter.focused,
      buttons = tasklist_buttons,
      -- style           = {
      --   shape_border_width = 1,
      --   shape_border_color = '#777777',
      --   shape              = gears.shape.rounded_bar,
      -- },
      layout = {
        spacing = 3,
        layout = wibox.layout.flex.horizontal
      },
      widget_template = {
        {
          {
            {
              {
                id = "icon_role",
                widget = wibox.widget.imagebox
              },
              margins = 2,
              widget = wibox.container.margin
            },
            {
              id = "text_role",
              widget = wibox.widget.textbox
            },
            layout = wibox.layout.fixed.horizontal
          },
          left = 10,
          right = 10,
          widget = wibox.container.margin
        },
        id = "background_role",
        forced_width = 230,
        widget = wibox.container.background
      }
    }

    -- Create the wibox
    s.mywibox = awful.wibar({position = "top", screen = s, height = 26})

    s.systray = wibox.widget.systray()
    s.systray.visible = false
    s.systray:set_base_size(21)
    -- Add widgets to the wibox
    s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      expand = "none",
      {
        -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        s.mytaglist,
        widgets.space,
        s.mypromptbox,
        s.mytasklist,
        widgets.space
      },
      nil, --widgets.mpris,
      {
        -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        id = "rightwidgets",
        -- widgets.kblayout,
        -- widgets.vol,
        widgets.textclock,
        wibox.layout.margin(s.systray, 3, 3, 3, 3),
        s.mylayoutbox
      }
    }
  end
)

-- Every bar theme should provide these fuctions
function tray_toggle()
  local s = awful.screen.focused()
  s.traybox.visible = not s.traybox.visible
end
