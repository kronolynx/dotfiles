Config {
      font               = "xft:FantasqueSansMono Nerd Font:pixelsize=19,Noto Sans CJK SC:pixelsize=18,Noto Sans CJK TC:pixelsize=18,Noto Sans CJK JP:pixelsize=18"
    , additionalFonts = [
      "xft:Noto Sans:size=10:style=Bold"
      ,"xft:FontAwesome:style=Regular:size=16"
      ]
    , bgColor            = "#2F343F"
    , alpha              = 255
    , fgColor            = "silver"
    , iconOffset = -8
    , position = TopSize C 100 30
    , textOffset = -1
    , border = BottomB
    , borderWidth      = 0
    , borderColor = "#c0c5ce"
    , lowerOnStart       = True
    , hideOnStart        = False
    , allDesktops        = True
    , overrideRedirect   = True
    , pickBroadest       = False
    , persistent         = False
    , alignSep           = "}{"
    , sepChar            = "%"
    , template = "%StdinReader% } %mpris2% { %locks% %kbd% %mad% <action=`pavucontrol` button=3><action=`~/.scripts/XMMute.sh` button=1><action=`xdotool key super+Up` button=4><action=`xdotool key super+Down` button=5>%XVol%</action></action></action></action> %battery% %date% %trayerpad%"
    -- plugins
    --   Numbers can be automatically colored according to their value. xmobar
    --   decides color based on a three-tier/two-cutoff system, controlled by
    --   command options:
    --     --Low sets the low cutoff
    --     --High sets the high cutoff
    --
    --     --low sets the color below --Low cutoff
    --     --normal sets the color between --Low and --High cutoffs
    --     --High sets the color above --High cutoff
    --
    --   The --template option controls how the plugin is displayed. Text
    --   color can be set by enclosing in <fc></fc> tags. For more details
    --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
    , commands =
      [
        Run Battery
           [ "--template", "<acstatus>"
            , "--Low"     , "10"
            , "--High"    , "80"
            , "--low"     , "red"
            , "--normal"  , "darkorange"
            , "--high"    , "purple"
            , "-f"        , "AC/online"
            , "--"
            , "-o"        , "<fc=#00e64d> <left></fc>"
            , "-O"        , "<fc=#00e64d> <left></fc>"
            , "-i"        , "<fc=#66FF99></fc>"
            ] 60
        -- cpu core temperature monitor
        , Run CoreTemp       
            [ "--template" , "± <core0>°±<core1>°"
            , "--Low"      , "70"        -- units: °C
            , "--High"     , "80"        -- units: °C
            , "--low"      , "#657b83"
            , "--normal"   , "#859900"
            , "--high"     , "#dc322f"
            ] 50
        , Run Network "wlp3s0"
            [ "-t", "<rxipat><txipat>    <fn=1>\xf1eb</fn>"
            , "-S", "Yes"
            , "-w", "6"
            , "-x", ""
            , "-a", "r"
            , "-H", "10000"
            , "-h", "#BF616A"
            , "-n", "#81A1C1"
            , "--"
            , "--rx-icon-pattern", "<fn=1>\x2595<rxvbar>\x258F</fn>"
            , "--tx-icon-pattern", "<fn=1>\x2595<txvbar>\x258F</fn>"
] 5
        -- LEMD is the ICAO code for Madrid/Barajas
        , Run Weather "LEMD" ["-t","<tempC>°C <skyCondition>","-L","16","-H","30","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000
        , Run Com "sh" ["-c", "mpc | head -n -2 | sed 's,.*/,,' | cut -c-40"] "mpd" 50
        , Run Date "<action=`~/.scripts/calendar.sh` button=1><fc=#00ff00> %a %b %d</fc></action><fc=#00a1f1> %H:%M:%S</fc>" "date" 10
        , Run Com "/bin/bash" ["-c", "~/.scripts/XMgetvolume.sh"]  "XVol" 10
        , Run Mpris2 "spotify" ["-t", "<artist> - <title>", "-x", ""] 10
        , Run Com "/bin/bash" ["-c", "~/.scripts/trayerpad.sh"]  "trayerpad" 10
        , Run Com "/bin/bash" ["-c", "~/.scripts/weather3.sh madrid"] "mad" 36000
        -- keyboard layout indicator
        , Run Kbd            
              [ ("us(dvorak)" , "<fc=#cc00ff> DV</fc>")
              , ("us"         , "<fc=#cc00ff> US</fc>")
              , ("es"         , "<fc=#cc00ff> ES</fc>")
              ]
        , Run Locks
        , Run StdinReader
        ]
}
