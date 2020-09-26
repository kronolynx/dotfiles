local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local icons = require("icons")

local helpers = require("helpers")

-- Appearance
local icon_size = beautiful.exit_screen_icon_size or dpi(140)
local text_font = beautiful.exit_screen_font or "sans 14"

-- Commands
local poweroff_command = function()
    awful.spawn.with_shell(user.logmenu .. " shutdown")
end
local reboot_command = function()
    awful.spawn.with_shell(user.logmenu .. " reboot")
end
local suspend_command = function()
    awful.spawn.with_shell(user.logmenu .. " suspend")
    exit_screen_hide()
end
local exit_command = function()
    awesome.quit()
end
local lock_command = function()
    awful.spawn.with_shell(user.logmenu .. " lock")
end

local username = os.getenv("USER")
-- Capitalize username
local goodbye_widget = wibox.widget.textbox("Goodbye " .. username:sub(1, 1):upper() .. username:sub(2))
goodbye_widget.font = "sans 70"

local create_button = function(image, button_text, hover_color, command)
    local button_icon = wibox.widget.imagebox(image)
    button_icon.resize = true
    button_icon.forced_width = icon_size
    button_icon.forced_height = icon_size
    local button_text = wibox.widget.textbox(button_text)
    button_text.font = text_font

    local button =
        wibox.widget {
        {
            nil,
            button_icon,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            nil,
            button_text,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        layout = wibox.layout.fixed.vertical
    }
    button:buttons(
        gears.table.join(
            awful.button(
                {},
                1,
                function()
                    command()
                end
            )
        )
    )

    -- Change color on hover
    button:connect_signal(
        "mouse::enter",
        function()
            button_text.markup = helpers.colorize_text(button_text.text, hover_color)
            button.border_color = hover_color
        end
    )
    button:connect_signal(
        "mouse::leave",
        function()
            button_text.markup = helpers.colorize_text(button_text.text, x.foreground)
            button.border_color = button_bg
        end
    )

    -- Use helper function to change the cursor on hover
    helpers.add_hover_cursor(button, "hand1")

    return button
end

local poweroff = create_button(icons.image.poweroff, "Poweroff", x.color1, poweroff_command)
local reboot = create_button(icons.image.reboot, "Reboot", x.color2, reboot_command)
local suspend = create_button(icons.image.suspend, "Suspend", x.color3, suspend_command)
local exit = create_button(icons.image.exit, "Exit", x.color4, exit_command)
local lock = create_button(icons.image.lock, "Lock", x.color5, lock_command)

-- Create the widget
exit_screen = wibox({visible = false, ontop = true, type = "dock"})
awful.placement.maximize(exit_screen)

exit_screen.bg = beautiful.exit_screen_bg or beautiful.wibar_bg or "#111111CC"
exit_screen.fg = beautiful.exit_screen_fg or beautiful.wibar_fg or "#FEFEFE"

local exit_screen_grabber
function exit_screen_hide()
    awful.keygrabber.stop(exit_screen_grabber)
    exit_screen.visible = false
end

local keybinds = {
    ["escape"] = exit_screen_hide,
    ["q"] = exit_screen_hide,
    ["x"] = exit_screen_hide,
    ["s"] = function()
        suspend_command()
        exit_screen_hide()
    end,
    ["e"] = exit_command,
    ["p"] = poweroff_command,
    ["r"] = reboot_command,
    ["l"] = function()
        lock_command()
        -- Kinda fixes the "white" (undimmed) flash that appears between
        -- exit screen disappearing and lock screen appearing
        gears.timer.delayed_call(
            function()
                exit_screen_hide()
            end
        )
    end
}

function exit_screen_show()
    exit_screen_grabber =
        awful.keygrabber.run(
        function(_, key, event)
            -- Ignore case
            key = key:lower()

            if event == "release" then
                return
            end

            if keybinds[key] then
                keybinds[key]()
            end
        end
    )
    exit_screen.visible = true
end

exit_screen:buttons(
    gears.table.join(
        -- Left click - Hide exit_screen
        awful.button(
            {},
            1,
            function()
                exit_screen_hide()
            end
        ),
        -- Middle click - Hide exit_screen
        awful.button(
            {},
            2,
            function()
                exit_screen_hide()
            end
        ),
        -- Right click - Hide exit_screen
        awful.button(
            {},
            3,
            function()
                exit_screen_hide()
            end
        )
    )
)

-- Item placement
exit_screen:setup {
    nil,
    {
        {
            nil,
            goodbye_widget,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            nil,
            {
                poweroff,
                reboot,
                suspend,
                exit,
                lock,
                spacing = dpi(20),
                layout = wibox.layout.fixed.horizontal
            },
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
            -- layout = wibox.layout.fixed.horizontal
        },
        layout = wibox.layout.fixed.vertical
    },
    nil,
    expand = "none",
    layout = wibox.layout.align.vertical
}
