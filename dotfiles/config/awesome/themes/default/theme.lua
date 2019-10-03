local os                                        = os

local theme_assets                              = require("beautiful.theme_assets")
local xresources                                = require("beautiful.xresources")
local dpi                                       = xresources.apply_dpi
local xrdb                                      = xresources.get_current_theme()

local theme                                     = {}

theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/default"

-- Colors
theme.xbackground                               = xrdb.background
theme.xforeground                               = xrdb.foreground
theme.xcolor0                                   = xrdb.color0
theme.xcolor1                                   = xrdb.color1
theme.xcolor2                                   = xrdb.color2
theme.xcolor3                                   = xrdb.color3
theme.xcolor4                                   = xrdb.color4
theme.xcolor5                                   = xrdb.color5
theme.xcolor6                                   = xrdb.color6
theme.xcolor7                                   = xrdb.color7
theme.xcolor8                                   = xrdb.color8
theme.xcolor9                                   = xrdb.color9
theme.xcolor10                                  = xrdb.color10
theme.xcolor11                                  = xrdb.color11
theme.xcolor12                                  = xrdb.color12
theme.xcolor13                                  = xrdb.color13
theme.xcolor14                                  = xrdb.color14
theme.xcolor15                                  = xrdb.color15
theme.wallpaper                                 = "~/.wallpapers/no-mans-sky.png"

-- Fonts
theme.font                                      = "FantasqueSansMono Nerd Font Regular 12"
theme.taglist_font                              = "FantasqueSansMono Nerd Font Bold 12"
theme.iconFont                                  = "FantasqueSansMono Nerd Font 12"

-- Standard Background and Foreground
theme.bg_normal                                 = theme.xbackground
theme.fg_normal                                 = theme.xforeground
theme.bg_focus                                  = theme.xcolor0
theme.fg_focus                                  = theme.xforeground
theme.bg_urgent                                 = theme.xcolor1
theme.fg_urgent                                 = theme.xcolor15

-- Borders
theme.border_width                              = dpi(2)
theme.border_normal                             = theme.xbackground
theme.border_focus                              = theme.xcolor7

-- Taglist
theme.taglist_bg_focus                          = theme.xcolor15
theme.taglist_fg_focus                          = theme.bg_normal
theme.taglist_bg_empty                          = theme.bg_normal
theme.taglist_fg_empty                          = theme.xcolor7
theme.taglist_fg_occupied                       = theme.xcolor15
theme.taglist_bg_occupied                       = theme.xcolor0
theme.taglist_bg_urgent                         = theme.xcolor1
theme.taglist_fg_urgent                         = theme.xcolor15
theme.taglist_spacing                           = dpi(0)

-- Generate taglist squares:
local taglist_square_size                       = dpi(3)
theme.taglist_squares_sel                       = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel                     = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

theme.awesome_icon                              = theme.dir .. "/icons/awesome.png"
-- Define the image to load
theme.titlebar_close_button_normal              = theme.dir .. "/icons/titlebar/close_normal.png"
theme.titlebar_close_button_focus               = theme.dir .. "/icons/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal           = theme.dir .. "/icons/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme.dir .. "/icons/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"


-- Layout icons
theme.layout_fairh                              = theme.dir .. "/icons/layouts/fairhw.png"
theme.layout_fairv                              = theme.dir .. "/icons/layouts/fairvw.png"
theme.layout_floating                           = theme.dir .. "/icons/layouts/floatingw.png"
theme.layout_magnifier                          = theme.dir .. "/icons/layouts/magnifierw.png"
theme.layout_max                                = theme.dir .. "/icons/layouts/maxw.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/layouts/fullscreenw.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/layouts/tilebottomw.png"
theme.layout_tileleft                           = theme.dir .. "/icons/layouts/tileleftw.png"
theme.layout_tile                               = theme.dir .. "/icons/layouts/tilew.png"
theme.layout_tiletop                            = theme.dir .. "/icons/layouts/tiletopw.png"
theme.layout_spiral                             = theme.dir .. "/icons/layouts/spiralw.png"
theme.layout_dwindle                            = theme.dir .. "/icons/layouts/dwindlew.png"
theme.layout_cornernw                           = theme.dir .. "/icons/layouts/cornernww.png"
theme.layout_cornerne                           = theme.dir .. "/icons/layouts/cornernew.png"
theme.layout_cornersw                           = theme.dir .. "/icons/layouts/cornersww.png"
theme.layout_cornerse                           = theme.dir .. "/icons/layouts/cornersew.png"

theme.useless_gap                               = dpi(3)
theme.screen_margin                             = dpi(5)
theme.maximized_hide_border                     = true

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme                                = nil

return theme
