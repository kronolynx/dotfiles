Config {
      font               = "xft:SauceCodePro Nerd Font:pixelsize=18"
    , bgColor            = "#222222"
    , alpha              = 0
    , fgColor            = "grey"
    , position           = TopW L 92
    , hideOnStart        = False
    , allDesktops        = True
    , overrideRedirect   = True
    , pickBroadest       = False
    , persistent         = True
    , alignSep           = "}{"
    , commands =
        [ Run BatteryP ["BAT0"]
            [ "--template", "<acstatus>"
            , "--Low"     , "10"
            , "--High"    , "80"
            , "--low"     , "red"
            , "--normal"  , "darkorange"
            , "--high"    , "darkgreen"
            , "-f"        , "AC/online"
            , "--"
                , "-o"  , " (<left>)"
                , "-O"  , "<fc=#00e64d> (<left>)</fc>"
                , "-i"  , "<fc=#66FF99></fc>"
            ] 60
        , Run Com "sh" ["-c", "awk '{print $1, $2, $3 }' /proc/loadavg"] "loadavg" 10
        -- , Run Com "sh" ["-c", "mpc | head -n -2 | sed 's,.*/,,' | cut -c-40"] "mpd" 50
        , Run Date "<fc=#00ff00> %a %b %d </fc><fc=#ee9a00> %H:%M:%S</fc>" "date" 10
        -- keyboard layout indicator
        , Run Kbd            [ ("us(dvorak)" , "<fc=#cc00ff>DV</fc>")
                             , ("us"         , "<fc=#cc00ff>US</fc>")
                             , ("es"         , "<fc=#cc00ff>ES</fc>")
                             ]
        , Run StdinReader
        ]
    , template = "%StdinReader%}{ <fc=#> %loadavg%</fc> %battery% %kbd% %date% "
}
