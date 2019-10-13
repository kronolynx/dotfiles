local beautiful = require("beautiful")
local gears     = require("gears")
local awful     = require("awful")

local helpers   = {}
-- Turn border color to back if there is only one client in the tag.
function helpers.border_adjust(c)
  if #awful.screen.focused().clients == 1 then
    c.border_color = beautiful.border_normal
  else
    c.border_color = beautiful.border_focus
  end
end

function helpers.set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then
      wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
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

return helpers
