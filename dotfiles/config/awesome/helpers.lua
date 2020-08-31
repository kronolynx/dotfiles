local beautiful = require("beautiful")
local gears     = require("gears")
local awful     = require("awful")

local helpers   = {}

function helpers.pad(size)
  local str = ""
  for i = 1, size do
      str = str .. " "
  end
  local pad = wibox.widget.textbox(str)
  return pad
end

-- Tag back and forth:
-- If you try to focus the same tag you are at, go back to the previous tag.
-- Useful for quick switching after for example checking an incoming chat
-- message at tag 2 and coming back to your work at tag 1 with the same
-- keypress
function helpers.tag_back_and_forth(tag_index)
  local s = mouse.screen
  local tag = s.tags[tag_index]
  if tag then
    if tag == s.selected_tag then
      awful.tag.history.restore()
    else
      tag:view_only()
    end
  end
end

function helpers.move_to_edge(c, direction)
  -- local workarea = awful.screen.focused().workarea
  -- local client_geometry = c:geometry()
  if direction == "up" then
    local old_x = c:geometry().x
    awful.placement.top(c, { honor_padding = true, honor_workarea = true, honor_padding = true })
    c.x = old_x
    -- c:geometry({ nil, y = workarea.y + beautiful.screen_margin * 2, nil, nil })
  elseif direction == "down" then
    local old_x = c:geometry().x
    awful.placement.bottom(c, { honor_padding = true, honor_workarea = true, honor_padding = true })
    c.x = old_x
    -- c:geometry({ nil, y = workarea.height + workarea.y - client_geometry.height - beautiful.screen_margin * 2 - beautiful.border_width * 2, nil, nil })
  elseif direction == "left" then
    local old_y = c:geometry().y
    awful.placement.left(c, { honor_padding = true, honor_workarea = true, honor_padding = true })
    c.y = old_y
    -- c:geometry({ x = workarea.x + beautiful.screen_margin * 2, nil, nil, nil })
  elseif direction == "right" then
    local old_y = c:geometry().y
    awful.placement.right(c, { honor_padding = true, honor_workarea = true, honor_padding = true })
    c.y = old_y
    -- c:geometry({ x = workarea.width + workarea.x - client_geometry.width - beautiful.screen_margin * 2 - beautiful.border_width * 2, nil, nil, nil })
  end
end


-- Add a hover cursor to a widget by changing the cursor on
-- mouse::enter and mouse::leave
-- You can find the names of the available cursors by opening any
-- cursor theme and looking in the "cursors folder"
-- For example: "hand1" is the cursor that appears when hovering over
-- links
function helpers.add_hover_cursor(w, hover_cursor)
  local original_cursor = "left_ptr"

  w:connect_signal("mouse::enter", function()
    local w = _G.mouse.current_wibox
    if w then
      w.cursor = hover_cursor
    end
  end)

  w:connect_signal("mouse::leave", function()
    local w = _G.mouse.current_wibox
    if w then
      w.cursor = original_cursor
    end
  end)
end

-- Move client DWIM (Do What I Mean)
-- Move to edge if the client / layout is floating
-- Swap by index if maximized
-- Else swap client by direction
function helpers.move_client_dwim(c, direction)
  if c.floating or (awful.layout.get(mouse.screen) == awful.layout.suit.floating) then
    helpers.move_to_edge(c, direction)
  elseif awful.layout.get(mouse.screen) == awful.layout.suit.max then
    if direction == "up" or direction == "left" then
      awful.client.swap.byidx(-1, c)
    elseif direction == "down" or direction == "right" then
      awful.client.swap.byidx(1, c)
    end
  else
    awful.client.swap.bydirection(direction, c, nil)
  end
end

function helpers.float_and_resize(c, width, height)
  c.width = width
  c.height = height
  awful.placement.centered(c,{honor_workarea=true, honor_padding = true})
  awful.client.property.set(c, 'floating_geometry', c:geometry())
  c.floating = true
  c:raise()
end

function helpers.colorize_text(txt, fg)
  return "<span foreground='" .. fg .."'>" .. txt .. "</span>"
end
-- Create rounded rectangle shape (in one line)
function helpers.rrect (radius)
  return function(cr, width, height)
      gears.shape.rounded_rect(cr, width, height, radius)
  end
end

function helpers.prrect(radius, tl, tr, br, bl)
  return function(cr, width, height)
      gears.shape.partially_rounded_rect(cr, width, height, tl, tr, br, bl, radius)
  end
end

local double_tap_timer = nil
function  helpers.single_double_tap(single_tap_function, double_tap_function)
    if double_tap_timer then
        double_tap_timer:stop()
        double_tap_timer = nil
        double_tap_function()
        -- naughty.notify({text = "We got a double tap"})
        return
    end

    double_tap_timer =
        gears.timer.start_new(0.20, function()
            double_tap_timer = nil
            -- naughty.notify({text = "We got a single tap"})
            if single_tap_function then
                single_tap_function()
            end
            return false
        end)
end

-- maximize all windows in a tag or restore layout used before maximizing
local previous_layout = {}
function helpers.toggle_full(c)
  if c == nil then return end
  local layout = awful.layout.get(c.screen) 
  local tag = awful.screen.focused().selected_tag
  if layout.name == layouts.max.name then
    local previous = previous_layout[tag.name] or layouts.tile.name
    local set_layout = layouts[previous] or layouts.tile
    awful.layout.set(layouts[previous])
  else
    previous_layout[tag.name] = layout.name
    awful.layout.set(layouts.max)
  end
  c:raise()
end

return helpers
