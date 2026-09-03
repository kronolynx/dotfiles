---------------------
---- MY PROGRAMS ----
---------------------

-- Set programs that you use
local terminal    = "kitty"
local fileManager = "dolphin"
local menu        = "vicinae toggle"

---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER" -- Sets "Windows" key as main modifier

-- Example binds, see https://wiki.hypr.land/Configuring/Basics/Binds/ for more
hl.bind(mainMod .. " + RETURN", hl.dsp.exec_cmd(terminal))
local closeWindowBind = hl.bind(mainMod .. "+ SHIFT + Q", hl.dsp.window.close())
closeWindowBind:set_enabled(true)
hl.bind(mainMod .. " + M", hl.dsp.exec_cmd("command -v hyprshutdown >/dev/null 2>&1 && hyprshutdown || hyprctl dispatch 'hl.dsp.exit()'"))
hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
-- hl.bind(mainMod .. " + R", hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + SPACE", hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + P", hl.dsp.window.pseudo())
hl.bind(mainMod .. " + S", hl.dsp.layout("togglesplit"))    -- dwindle only

-- TODO testing
hl.bind(mainMod .. " + F", hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }), { description = "Toggle Fullscreen" })
hl.bind(mainMod .. " + A", hl.dsp.window.fullscreen({ mode = "maximized", action = "toggle" }), { description = "Toggle Maximize Window" })
-- hl.bind(mainMod .. " + G", hl.dsp.group.toggle(), { description = "Toggle window group" })

-- Monocle-style focus, like xmonad's Full layout.
--
-- While a window is maximized (SUPER+A) or fullscreen (SUPER+F), directional
-- focus does nothing at all: the focused window covers the whole area, so
-- geometrically there is no window to its left or right. In that state, step
-- through the workspace's tiled windows instead and re-apply the same mode to
-- whichever one gains focus, so the windows hidden behind come to the front one
-- at a time.
--
-- Hyprland only allows one maximized/fullscreen window per workspace, so
-- setting the mode on the new window clears it on the old one by itself.
---@param direction string  direction to use when not maximized
---@param step number       +1 for the next window in the stack, -1 for the previous
---@return nil
local function focus_or_cycle(direction, step)
    local active = hl.get_active_window()
    -- 0 = normal, 1 = maximized, 2 = fullscreen
    local mode = active and active.fullscreen or 0

    if mode == 0 then
        hl.dispatch(hl.dsp.focus({ direction = direction }))
        return
    end

    -- Tiled windows on this workspace, in Hyprland's own order.
    local windows = {}
    for _, window in ipairs(hl.get_windows()) do
        if not window.floating and window.workspace and window.workspace.name == active.workspace.name then
            table.insert(windows, window)
        end
    end

    if #windows < 2 then
        return
    end

    local current = 1
    for i, window in ipairs(windows) do
        if window.address == active.address then
            current = i
            break
        end
    end

    -- Lua's % is never negative for a positive divisor, so this wraps both ways.
    local target = windows[(current - 1 + step) % #windows + 1]

    hl.dispatch(hl.dsp.focus({ window = "address:" .. target.address }))
    hl.dispatch(hl.dsp.window.fullscreen({
        mode = mode == 2 and "fullscreen" or "maximized",
        action = "set",
    }))
end

-- Move focus with mainMod + arrow keys
hl.bind(mainMod .. " + left",  function() focus_or_cycle("left", -1) end,  { description = "Focus left / previous when maximized" })
hl.bind(mainMod .. " + right", function() focus_or_cycle("right", 1) end,  { description = "Focus right / next when maximized" })
hl.bind(mainMod .. " + up",    function() focus_or_cycle("up", -1) end,    { description = "Focus up / previous when maximized" })
hl.bind(mainMod .. " + down",  function() focus_or_cycle("down", 1) end,   { description = "Focus down / next when maximized" })

-- Move focus
hl.bind(mainMod .. " + h", function() focus_or_cycle("left", -1) end,  { description = "Focus left / previous when maximized" })
hl.bind(mainMod .. " + l", function() focus_or_cycle("right", 1) end,  { description = "Focus right / next when maximized" })
hl.bind(mainMod .. " + k", function() focus_or_cycle("up", -1) end,    { description = "Focus up / previous when maximized" })
hl.bind(mainMod .. " + j", function() focus_or_cycle("down", 1) end,   { description = "Focus down / next when maximized" })

hl.bind(mainMod .. " + S", hl.dsp.layout("swapsplit"), { description = "Swapsplit" })
hl.bind(mainMod .. " + SHIFT + h", hl.dsp.window.swap({ direction = "l" }), { description = "Swap tiled window left" })
hl.bind(mainMod .. " + SHIFT + l", hl.dsp.window.swap({ direction = "r" }), { description = "Swap tiled window right" })
hl.bind(mainMod .. " + SHIFT + k", hl.dsp.window.swap({ direction = "u" }), { description = "Swap tiled window up" })
hl.bind(mainMod .. " + SHIFT + j", hl.dsp.window.swap({ direction = "d" }), { description = "Swap tiled window down" })

hl.bind(mainMod .. " + ALT + left", hl.dsp.window.swap({ direction = "l" }), { description = "Swap tiled window left" })
hl.bind(mainMod .. " + ALT + right", hl.dsp.window.swap({ direction = "r" }), { description = "Swap tiled window right" })
hl.bind(mainMod .. " + ALT + up", hl.dsp.window.swap({ direction = "u" }), { description = "Swap tiled window up" })
hl.bind(mainMod .. " + ALT + down", hl.dsp.window.swap({ direction = "d" }), { description = "Swap tiled window down" })

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
    hl.bind(mainMod .. " + SHIFT + " .. key,     hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true, description  = "Move window with the mouse" })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true, description = "Resize window with the mouse" })

-- Laptop multimedia keys for volume and LCD brightness
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),      { locked = true, repeating = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),     { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",     hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),   { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",  hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%+"),                  { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown",hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%-"),                  { locked = true, repeating = true })

-- Requires playerctl
hl.bind("XF86AudioNext",  hl.dsp.exec_cmd("playerctl next"),       { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",  hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev",  hl.dsp.exec_cmd("playerctl previous"),   { locked = true })

-- Switch to a submap called `resize`.
hl.bind("ALT + R", hl.dsp.submap("resize"))

hl.define_submap("resize", function()

    -- Set repeating binds for resizing the active window.
    hl.bind("l", hl.dsp.window.resize({ x = 10, y = 0, relative = true}), { repeating = true })
    hl.bind("h", hl.dsp.window.resize({ x = -10, y = 0, relative = true}), { repeating = true })
    hl.bind("k", hl.dsp.window.resize({ x = 0, y = 10, relative = true}), { repeating = true })
    hl.bind("j", hl.dsp.window.resize({ x = 0, y = -10, relative = true}), { repeating = true })

    -- Use `reset` to go back to the global submap
    hl.bind("escape", hl.dsp.submap("reset"))

end)

local MAX_ZOOM = 3
local MIN_ZOOM = 1
local ZOOM_TOGGLE_FACTOR = 1.5

---@param offset number
---@return nil
local function zoom(offset)
    local current = hl.get_config("cursor.zoom_factor")
    if offset ~= nil then
        current = current + offset
    elseif current ~= MIN_ZOOM then
        current = MIN_ZOOM
    else
        current = ZOOM_TOGGLE_FACTOR
    end
    current = math.max(MIN_ZOOM, math.min(MAX_ZOOM, current))
    hl.config({ cursor = { zoom_factor = current } })
end

hl.bind("SUPER + Z", zoom)
hl.bind("SUPER + KP_ADD", function()
    zoom(0.5)
end)
hl.bind("SUPER + minus", function()
    zoom(-0.5)
end)

-- TODO
-- hl.bind("SUPER + ", function()
--     hl.dispatch(hl.dsp.window.cycle_next({
--         floating = not hl.get_active_window().floating
--     }))
-- end, { description = "Switch focus between tiled and floating windows" })
hl.bind("SUPER + G", hl.dsp.submap("group_management"), { description = "Enter a group management submap" })

local map = function(key, action, description)
    hl.bind(key, function()
        hl.dispatch(action)
        hl.dispatch(hl.dsp.submap("reset"))
    end, { description = description })
end

hl.define_submap("group_management", function()
    map("g", hl.dsp.group.toggle(), "Toggle window group")

    map("h", hl.dsp.window.move({ into_group = "l" }), "Move window into a group on the left")
    map("j", hl.dsp.window.move({ into_group = "d" }), "Move window into a group on the bottom")
    map("k", hl.dsp.window.move({ into_group = "u" }), "Move window into a group on the top")
    map("l", hl.dsp.window.move({ into_group = "r" }), "Move window into a group on the right")

    map("e", hl.dsp.window.move({ out_of_group = true }), "Move window out of group")

    map("n", hl.dsp.group.next(), "Next window in group")
    map("p", hl.dsp.group.prev(), "Previous window in group")

    map("f", hl.dsp.group.move_window(), "Move window forward in the group order")
    map("b", hl.dsp.group.move_window({ forward = false }), "Move window backward in the group order")

    map("t", hl.dsp.group.lock_active(), "Toggle group lock")

    for i = 1, 10 do
        map(tostring(i % 10), hl.dsp.group.active({ index = i }), "Focus window " .. i .. " in a group")
    end

    hl.bind("escape", hl.dsp.submap("reset"), { description = "Quit submap" })
end)