import           System.IO
import           System.Exit

import           XMonad
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FloatKeys
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FloatNext
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Util.NamedScratchpad
import           XMonad.Layout
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.Spacing
import           XMonad.Util.Loggers
import           XMonad.Util.Font
import           XMonad.Util.Run
import           Graphics.X11.ExtraTypes.XF86

import           XMonad.Util.SpawnOnce
import           Data.Char
import           Data.Bits ((.|.))
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- import qualified Data.Text as T

-- DefaultTerminal: Set to succless terminal (Alacritty, Termite)
myTerminal = "termite"
-- myTerminal = "xterm"

-- Launcher: Set to Rofi (x: 380)
myLauncher = "rofi -show drun"

-- Browser
myBrowser = "google-chrome-stable"

-- File Manager
myFileManager = "pcmanfm"

-- Console File Manager
myConsoleFileManager = "termite -e ranger"


-- Border Styling
myBorderWidth = 1

myNormalBorderColor = "#BFBFBF"

myFocusedBorderColor = "#CAA9FA"

myXmobar = "xmobar ~/.config/xmobar/xmobarrc.hs"

-- Xmobar dyn colors
xmobarCurrFG = "#282A36"

xmobarCurrBG = "#FF79C6"

xmobarHiddenFG = "#747C84"

workspaceIcons =
  [ "\xe17e"
  , "\xe17f"
  , "\xe180"
  , "\xe181"
  , "\xe182"
  , "\xe183"
  , "\xe184"
  , "\xe185"
  , "\xe186"
  ]

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
workspaceNames =
  [ "Main"
  , "Qutebrowser"
  , "Code"
  , "Firefox"
  , "Chrome"
  , "Media"
  , "Misc7"
  , "Misc8"
  , "Misc9"
  ]

myWorkspaces = wrapWorkspaces workspaceIcons $ workspaceNames
 where
  wrapWorkspaces icons workspaces =
    [ "<fn=1>" ++ i ++ "</fn> " ++ ws | (i, ws) <- zip icons workspaces ]

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- myManageHook
myManageHook = composeAll
  [ className =? "qutebrowser" --> doShift " Qutebrowser"
  , className =? "Spotify" --> doShift " Media"
  , className =? "Firefox" --> doShift " Firefox"
  , className =? "Chromium" --> doShift " Chrome"
  ]

scratchpads =
  [ NS "htop" "termite -t process -e htop" (title =? "process")  defaultFloating
  , NS "cmus" "termite -c cmus -e cmus"    (className =? "cmus") defaultFloating
  ]

myNewManageHook = composeAll
  [ myManageHook
  , floatNextHook
  , manageHook desktopConfig
  , namedScratchpadManageHook scratchpads
  ]


myStartupHook = do
  --spawnOnce " /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  --spawnOnce "pulseaudio --start"
  --spawnOnce "pa-applet"
  -- spawnOnce "urxvtd --quiet --opendisplay --fork &"
  spawn "xfce4-power-manager"
  spawn "emacs --daemon"
  spawn "thunar --daemon"
  spawn "nitrogen --restore ~/.wallpapers"
  spawn "nm-applet"
  spawn "compton -f"
  spawn "clipit"
  spawn "~/.scripts/keyboard.sh"
  spawn "~/.scripts/monitor.sh"
  -- spawn "stalonetray"
  spawn "xautolock -time 10 -locker blurlock"
  spawn "sh -c 'sleep 10; exec redshift -c ~/.config/redshift/redshift.conf'"
  spawn "sh -c 'sleep 40; exec keepassxc'"
  spawn "sh -c 'sleep 60; exec telegram'"
  spawn "sh -c 'sleep 180; exec megasync'"

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modMask }) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn myTerminal)

  -- Spawn the launcher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Start browser.
  , ((modMask, xK_F2),
     spawn myBrowser)

  -- Start file manager.
  , ((modMask, xK_F3),
     spawn myFileManager)

  -- Start filemanager in terminal
  , ((modMask, xK_F4),
     spawn myConsoleFileManager)

  -- Kill window
  , ((modMask .|. controlMask, xK_x),
     spawn "xkill")

  -- Close focused window
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "amixer -q set Master 5%-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "amixer -q set Master 5%+")

  -- Music bindings
  , ((0, xF86XK_AudioNext),
     spawn "playerctl next")
  , ((0, xF86XK_AudioPrev),
     spawn "playerctl previous")
  , ((0, xF86XK_AudioPlay),
     spawn "playerctl play-pause")
  , ((0, xF86XK_AudioStop),
     spawn "playerctl stop")


  -- Scratchpads
  , ((modMask .|. controlMask .|. shiftMask, xK_t),
      namedScratchpadAction scratchpads "htop")
  , ((modMask .|. controlMask .|. shiftMask, xK_c),
      namedScratchpadAction scratchpads "cmus")
  ]

  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- my TEXT manipulation functions ... no longer of use, but might
--    come in handy later, so leaving here ....
-- splitH s = T.splitOn (T.pack " ") (T.pack s)
-- splitAndDropFst = tail . splitH
-- splitAndTakeFst = take 1 . splitH
-- splitMapJoin fn s = unwords (fmap T.unpack (fn s))

splitTakeFst = head . words

splitTakeLst = last . words

prependIcon = (++) "<fn=1>\xe194</fn> " . splitTakeLst

prependWSLogger = fmap prependIcon <$> logCurrent

-- getShortenedLayout = fmap splitTakeLst <$> logLayout

myXmobarPP = def { ppCurrent = xmobarColor xmobarCurrBG "" . splitTakeFst
                 , ppHidden  = xmobarColor xmobarHiddenFG "" . splitTakeFst
                 , ppSep     = " "
                 , ppWsSep   = " "
                 , ppExtras  = [prependWSLogger]
                 , ppTitle   = const ""
                 , ppLayout  = const ""
                 }

main :: IO ()
main = do
  spawn myXmobar
  xmonad $ defaults { logHook = dynamicLogString myXmobarPP >>= xmonadPropLog }

defaults = docks $ desktopConfig
  { borderWidth        = myBorderWidth
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , modMask            = myModMask
  , terminal           = myTerminal
  , workspaces         = myWorkspaces
  , keys               = myKeys
  , manageHook         = myNewManageHook
  , layoutHook = avoidStruts $ smartBorders $ smartSpacingWithEdge 8 $ myLayout
  , startupHook        = myStartupHook
  }
