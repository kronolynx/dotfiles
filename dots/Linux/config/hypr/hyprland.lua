require("keybinds")
require("animations")

local terminal = "kitty"

------------------
---- MONITORS ----
------------------

-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
hl.monitor({
    output   = "",
    mode     = "preferred",
    position = "auto",
    scale    = "auto",
})



---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout    = "us",
        kb_variant   = ",intl",
        kb_model     = "",
        kb_options   = "",
        kb_rules     = "",

        follow_mouse = 1,

        sensitivity  = -0.3, -- -1.0 - 1.0, 0 means no modification.

        touchpad     = {
            natural_scroll = false,
        },
    },
})

hl.gesture({
    fingers = 3,
    direction = "horizontal",
    action = "workspace"
})

-- Example per-device config
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/ for more
-- hl.device({
--     name        = "epic-mouse-v1",
--     sensitivity = -0.5,
-- })

-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
-- Qt/KDE theming. This used to say qt6ct + Kvantum, but neither package is
-- installed, so Qt fell back to its built-in defaults: no icon theme, no colour
-- scheme, no fonts -- which is why dolphin looked unthemed. QT_STYLE_OVERRIDE
-- was the worse half, since it overrides whatever the platform theme picks and
-- pointed at a style that does not exist.
--
-- "kde" is the plasma-integration plugin (KDEPlasmaPlatformTheme6.so, already
-- installed), which reads ~/.config/kdeglobals -- the same colours, fonts and
-- icons the KDE session used.
--
-- QT_STYLE_OVERRIDE is set to Breeze rather than deleted on purpose: `hyprctl
-- reload` can only add or change env vars, never unset them, so dropping the
-- line would leave the stale Kvantum value in place for the rest of the
-- session. Breeze is the style plasma-integration would pick anyway, and it is
-- installed (breeze6.so).
hl.env("QT_QPA_PLATFORMTHEME", "kde")
hl.env("QT_STYLE_OVERRIDE", "Breeze")
hl.env("QT_QPA_PLATFORM", "wayland")
hl.env("MOZ_ENABLE_WAYLAND", "1")

-- The display manager starts this session with XDG_DATA_DIRS unset, and
-- quickshell does not fall back to the spec default when it is missing: its
-- DesktopEntries index comes up empty and the app icons on the workspace pills
-- never resolve. Set it only when the session did not provide one.
if not os.getenv("XDG_DATA_DIRS") then
    hl.env("XDG_DATA_DIRS", "/usr/local/share:/usr/share")
end

-- SeFu2FE&^2k**H


-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

-- Autostart necessary processes (like notifications daemons, status bars, etc.)
-- Or execute your favorite apps at launch like this:
--
hl.on("hyprland.start", function()
    hl.exec_cmd(terminal)
    hl.exec_cmd("systemctl --user start hyprpolkitagent")
    hl.exec_cmd("wl-paste --watch cliphist store")
    hl.exec_cmd("swaync")
    -- Locks the session before suspend/hibernate and after 10 minutes idle.
    -- Without this nothing ever started hyprlock, so resuming from suspend went
    -- straight back to the desktop. See hypridle.conf / hyprlock.conf.
    hl.exec_cmd("hypridle")
    -- Bar and wallpaper (~/.config/quickshell). This replaces swaybg, which
    -- was pointed at $XDG_CONFIG_HOME/.wallpapers -- unset in Hyprland's env,
    -- so it resolved to /.wallpapers and drew nothing. The wallpaper path now
    -- lives in quickshell/theme/Config.qml. Network and bluetooth are cells in
    -- the bar, so nm-applet is gone too (it was never installed anyway).
    hl.exec_cmd("qs -d")
    hl.exec_cmd("vicinae server")
    -- Export variables to systemd
    hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_DATA_DIRS")

    -- Restart portals so they catch the environment
    hl.exec_cmd("systemctl --user stop xdg-desktop-portal xdg-desktop-portal-hyprland")
    hl.exec_cmd("systemctl --user start xdg-desktop-portal-hyprland xdg-desktop-portal")
end)


-----------------------
----- PERMISSIONS -----
-----------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Permissions/
-- Please note permission changes here require a Hyprland restart and are not applied on-the-fly
-- for security reasons

hl.config({
    ecosystem = {
        enforce_permissions = true,
        no_update_news = true, -- Disable the popup that shows up after an update.
        no_donation_nag = true, -- Disable the popup that asks for donations.
    },
})

-- hyprlock binds the screencopy protocol at startup (it is what backs its
-- `path = screenshot` background option), and with enforce_permissions on that
-- triggers the "an application is trying to capture your screen" prompt every
-- time the session locks. Allowing it outright is safe here: the binary is
-- root-owned and package-managed, and it is the lock screen itself.
hl.permission("/usr/(bin|local/bin)/hyprlock", "screencopy", "allow")

-- Uncomment to stop screenshots and screen sharing being blocked by the same
-- mechanism -- the portal rule covers browsers, OBS and any screenshot tool
-- that goes through xdg-desktop-portal, the grim rule covers grim/slurp
-- hl.permission("/usr/(bin|local/bin)/grim", "screencopy", "allow")
-- hl.permission("/usr/(lib|libexec|lib64)/xdg-desktop-portal-hyprland", "screencopy", "allow")
-- hl.permission("/usr/(bin|local/bin)/hyprpm", "plugin", "allow")


-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
    general = {
        gaps_in          = 4,
        gaps_out         = 4,

        border_size      = 2,

        col              = {
            active_border   = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        -- Set to true to enable resizing windows by clicking and dragging on borders and gaps
        resize_on_border = true,

        -- Please see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/ before you turn this on
        allow_tearing    = false,

        layout           = "dwindle",
    },

    decoration = {
        rounding         = 10,
        rounding_power   = 2,

        -- Change transparency of focused and unfocused windows
        active_opacity   = 1.0,
        inactive_opacity = 1.0,

        shadow           = {
            enabled      = true,
            range        = 4,
            render_power = 3,
            color        = 0xee1a1a1a,
        },

        blur             = {
            enabled  = true,
            size     = 3,
            passes   = 1,
            vibrancy = 0.1696,
        },
    },

    animations = {
        enabled = true,
    },
})

-- Ref https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/
-- "Smart gaps" / "No gaps when only"
-- uncomment all if you wish to use that.
-- hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
-- hl.workspace_rule({ workspace = "f[1]",   gaps_out = 0, gaps_in = 0 })
-- hl.window_rule({
--     name  = "no-gaps-wtv1",
--     match = { float = false, workspace = "w[tv1]" },
--     border_size = 0,
--     rounding    = 0,
-- })
-- hl.window_rule({
--     name  = "no-gaps-f1",
--     match = { float = false, workspace = "f[1]" },
--     border_size = 0,
--     rounding    = 0,
-- })

-- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more
hl.config({
    dwindle = {
        preserve_split = true, -- You probably want this
    },
})

-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
hl.config({
    master = {
        new_status = "master",
    },
})

-- See https://wiki.hypr.land/Configuring/Layouts/Scrolling-Layout/ for more
hl.config({
    scrolling = {
        fullscreen_on_one_column = true,
    },
})

----------------
----  MISC  ----
----------------

hl.config({
    misc = {
        force_default_wallpaper = -1,   -- Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo   = true, -- If true disables the random hyprland logo / anime girl background. :(

        -- Wake up with key/mouse activity:
        mouse_move_enables_dpms = true,
        key_press_enables_dpms  = true,
    },
})



--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Example window rules that are useful

local suppressMaximizeRule = hl.window_rule({
    -- Ignore maximize requests from all apps. You'll probably like this.
    name           = "suppress-maximize-events",
    match          = { class = ".*" },

    suppress_event = "maximize",
})
-- suppressMaximizeRule:set_enabled(false)

hl.window_rule({
    -- Fix some dragging issues with XWayland
    name     = "fix-xwayland-drags",
    match    = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },

    no_focus = true,
})

-- Layer rules also return a handle.
-- local overlayLayerRule = hl.layer_rule({
--     name  = "no-anim-overlay",
--     match = { namespace = "^my-overlay$" },
--     no_anim = true,
-- })
-- overlayLayerRule:set_enabled(false)

-- Hyprland-run windowrule
hl.window_rule({
    name  = "move-hyprland-run",
    match = { class = "hyprland-run" },

    move  = "20 monitor_h-120",
    float = true,
})
