Config {
      font               = "xft:FantasqueSansMono Nerd Font:pixelsize=19,Noto Sans CJK SC:pixelsize=18,Noto Sans CJK TC:pixelsize=18,Noto Sans CJK JP:pixelsize=18"
    , additionalFonts = ["xft:Noto Sans:size=10:style=Bold","xft:FontAwesome:style=Regular:size=13"]
    , bgColor            = "#2F343F"
    , alpha              = 255
    , fgColor            = "grey"
    , iconOffset = -8
    , position = TopSize C 100 30
    -- , position           = TopW L 92
    , textOffset = -1
    , border = BottomB
    , borderWidth      = 0
    , borderColor = "#c0c5ce"
    , lowerOnStart       = True
    , hideOnStart        = False
    , allDesktops        = True
    , overrideRedirect   = True
    , pickBroadest       = False
    , persistent         = True
    , alignSep           = "}{"
    , sepChar            = "%"
    , commands =
      [
        Run Battery
           [ "--template", "<acstatus>"
            , "--Low"     , "10"
            , "--High"    , "80"
            , "--low"     , "red"
            , "--normal"  , "darkorange"
            , "--high"    , "darkgreen"
            , "-f"        , "AC/online"
            , "--"
            , "-o"        , "(<left>)"
            , "-O"        , "<fc=#00e64d>(<left>)</fc>"
            , "-i"        , "<fc=#66FF99></fc>"
            ] 60
        --, Run Wireless           "wlp0s20u2u3"        ["-t", "<fc=green>Wifi:</fc> <quality>"
--                                                ] 10
        -- , Run Com "sh" ["-c", "mpc | head -n -2 | sed 's,.*/,,' | cut -c-40"] "mpd" 50
        , Run Date "<fc=#00ff00> %a %b %d</fc><fc=#ee9a00> %H:%M:%S</fc>" "date" 10

        , Run Com "/bin/bash" ["-c", "XMgetvolume"]  "XVol" 10
        , Run Mpris2 "spotify" ["-t", "<artist> - <title>"] 10
        , Run Com "/bin/bash" ["-c", "trayerpad"]  "trayerpad" 10
        -- keyboard layout indicator
        , Run Kbd            [ ("us(dvorak)" , "<fc=#cc00ff> DV</fc>")
                             , ("us"         , "<fc=#cc00ff> US</fc>")
                             , ("es"         , "<fc=#cc00ff> ES</fc>")
                             ]
        , Run StdinReader
        ]
    , template = "%StdinReader%} %mpris2% { %kbd% %date% <action=`pavucontrol` button=3><action=`XMMute` button=1><action=`xdotool key super+Up` button=4><action=`xdotool key super+Down` button=5>%XVol%</action></action></action></action> %battery% %trayerpad%"
}