local beautiful = require("beautiful")
local wibox     = require("wibox")
local awful     = require("awful")
local gears = require("gears")
local helpers   = require("helpers")

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
  -- Start the new client as slave.
  if not awesome.startup then
    awful.client.setslave(c)
  end

  if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c, { honor_padding = true })
  end

  if c.type == 'dialog' then
    local t = awful.screen.focused().selected_tag
    c:move_to_tag(t)
  end
  -- When a client starts up in fullscreen, resize it to cover the fullscreen a short moment later
  -- Fixes wrong geometry when titlebars are enabled
  if c.fullscreen then
      gears.timer.delayed_call(function()
          if c.valid then
              c:geometry(c.screen.geometry)
          end
      end)
  end

  if awful.layout.get(mouse.screen) == awful.layout.suit.floating then
      awful.client.property.set(c, 'floating_geometry', c:geometry())
  end
end)

-- Force minimized clients to unminimize.
client.connect_signal("property::minimized", function(c)
  c.minimized = false
end)

client.connect_signal("property::maximized", function(c)
  awful.titlebar.hide(c)
  -- c.border_width = 0 
end)

-- screen.connect_signal("arrange", function (s)
--     local max = s.selected_tag.layout.name == "max"
--     local only_one = #s.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count
--     -- but iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
--     for _, c in pairs(s.clients) do
--         if (max or only_one) and not c.floating or c.maximized then
--             c.border_width = 0
--         else
--             c.border_width = beautiful.border_width
--         end
--     end
-- end)


-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
  if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
      and awful.client.focus.filter(c) then
    client.focus = c
  end
end)

client.connect_signal("focus", function(c)
    c.border_color = beautiful.border_focus
end)

client.connect_signal("unfocus", function(c)
  c.border_color = beautiful.border_normal
end)


-- -- Raise focused clients automatically
-- client.connect_signal("focus", function(c) c:raise() end)

tag.connect_signal("property::layout", function(t)
    local clients = t:clients()
    for k,c in pairs(clients) do
        if c.floating or c.first_tag.layout.name == "floating" then
            awful.titlebar.show(c)
            c.border_width = 0 
        else
            awful.titlebar.hide(c)
            c.border_width = beautiful.border_width
        end
        -- Restore geometry for floating clients
        -- (for example after swapping from tiling mode to floating mode)
        if awful.layout.get(mouse.screen) == awful.layout.suit.floating then
            local cgeo = awful.client.property.get(c, 'floating_geometry')
            if cgeo then
                c:geometry(awful.client.property.get(c, 'floating_geometry'))
            end
        end
    end
end)

-- Set mouse resize mode (live or after)
awful.mouse.resize.set_mode("live")



client.connect_signal('property::geometry', function(c)
    if awful.layout.get(mouse.screen) == awful.layout.suit.floating then
        awful.client.property.set(c, 'floating_geometry', c:geometry())
    end
end)

-- Disable ontop when the client is not floating, and restore ontop if needed
-- when the client is floating again
-- I never want a non floating client to be ontop.
client.connect_signal('property::floating', function(c)
    if c.floating then
        if c.restore_ontop then
            c.ontop = c.restore_ontop
        end
    else
        c.restore_ontop = c.ontop
        c.ontop = false
    end
    -- Titlebars only on floating windows
    if c.floating then
        awful.titlebar.show(c)
        c.border_width = 0 
    else
        awful.titlebar.hide(c)
        c.border_width = beautiful.border_width
    end
end)

-- Remove the border for maximized windows
client.connect_signal("property::maximized", function(client)
    if client.maximized then
        client.border_width = 0
        client.shape = nil
    else
        -- Restore it again
        client.border_width = beautiful.border_width
        client.shape = beautiful.border_shape
    end
end)

-- When switching to a tag with urgent clients, raise them.
-- This fixes the issue (visual mismatch) where after switching to
-- a tag which includes an urgent client, the urgent client is
-- unfocused but still covers all other windows (even the currently
-- focused window).
awful.tag.attached_connect_signal(s, "property::selected", function ()
    local urgent_clients = function (c)
        return awful.rules.match(c, { urgent = true })
    end
    for c in awful.client.iterate(urgent_clients) do
        if c.first_tag == mouse.screen.selected_tag then
            client.focus = c
        end
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = gears.table.join(
      awful.button({ }, 1, function()
        client.focus = c
        c:raise()
        awful.mouse.client.move(c)
      end),
      awful.button({ }, 3, function()
        client.focus = c
        c:raise()
        awful.mouse.client.resize(c)
      end)
  )

  awful.titlebar(c):setup {
    { -- Left
      -- awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Title
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
      -- awful.titlebar.widget.floatingbutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      -- awful.titlebar.widget.stickybutton(c),
      -- awful.titlebar.widget.ontopbutton(c),
      awful.titlebar.widget.closebutton(c),
      layout = wibox.layout.fixed.horizontal()
    },
    layout = wibox.layout.align.horizontal
  }
end)
