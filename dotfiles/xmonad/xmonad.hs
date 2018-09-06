{-# LANGUAGE UnicodeSyntax #-}
import XMonad
import XMonad.Config.Desktop
import System.Exit

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FloatNext

-- layouts
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ToggleLayouts     -- Full window at any time
import XMonad.Layout.Fullscreen

-- utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

-- prompt
import XMonad.Prompt
import XMonad.Prompt.Window            -- pops up a prompt with window names

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys

-- Keys
import XMonad.Util.EZConfig            -- removeKeys, additionalKeys
import Graphics.X11.ExtraTypes.XF86

import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
  xmonad $ defaults {
    manageHook = manageDocks <+> manageHook defaultConfig,
    -- layoutHook = avoidStruts $ layoutHook defaultConfig,
    logHook = dynamicLogWithPP $ xmobarPP
                        {
                          ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "green" "" . shorten 50,
                          ppSep = " "
                        }
  }
       -------------------------------------------------------------------- }}}
       -- Define keys to remove                                             {{{
       ------------------------------------------------------------------------

       `removeKeysP`
       [
       -- Unused gmrun binding
          "M-S-p"
       -- Unused close window binding
       ,  "M-S-c"
       ]

       -------------------------------------------------------------------- }}}
       -- Keymap: window operations                                         {{{
       ------------------------------------------------------------------------

       `additionalKeysP`
       [
       -- Shrink / Expand the focused window
         ("M-,"    , sendMessage Shrink)
       , ("M-."    , sendMessage Expand)
       -- Toggle struts
       , ("M-b"    , sendMessage ToggleStruts)
       -- Close the focused window
       , ("M-S-q"  , kill)
       -- Toggle layout (Fullscreen mode)
       , ("M-f"    , sendMessage (Toggle "Full"))
       -- Float window
       , ("M-S-f"  , withFocused (keysMoveWindow (-myBorderWidth,-myBorderWidth)))
       -- Push window back into tilling
       , ("M-S-t"  , withFocused $ windows . W.sink)
       -- Move the floating focused window
       , ("M-C-<R>", withFocused (keysMoveWindow (moveWD, 0)))
       , ("M-C-<L>", withFocused (keysMoveWindow (-moveWD, 0)))
       , ("M-C-<U>", withFocused (keysMoveWindow (0, -moveWD)))
       , ("M-C-<D>", withFocused (keysMoveWindow (0, moveWD)))
       -- Resize the floating focused window
       , ("M-s"    , withFocused (keysResizeWindow (-resizeWD, resizeWD) (0.5, 0.5)))
       , ("M-i"    , withFocused (keysResizeWindow (resizeWD, resizeWD) (0.5, 0.5)))
       -- Increase / Decrese the number of master pane
       , ("M-S-,"  , sendMessage $ IncMasterN (-1))
       , ("M-S-."  , sendMessage $ IncMasterN 1)
       -- Go to the next / previous workspace
       , ("M-<R>"  , nextWS )
       , ("M-<L>"  , prevWS )
       , ("M-l"    , nextWS )
       , ("M-h"    , prevWS )
       -- Shift the focused window to the next / previous workspace
       , ("M-S-<R>", shiftToNext)
       , ("M-S-<L>", shiftToPrev)
       , ("M-S-l"  , shiftToNext)
       , ("M-S-h"  , shiftToPrev)
       -- Move the focus down / up
       , ("M-<D>"  , windows W.focusDown)
       , ("M-<U>"  , windows W.focusUp)
       , ("M-j"    , windows W.focusDown)
       , ("M-k"    , windows W.focusUp)
       -- Swap the focused window down / up
       , ("M-S-j"  , windows W.swapDown)
       , ("M-S-k"  , windows W.swapUp)
       -- Shift the focused window to the master window
       , ("M-m"    , windows W.shiftMaster)
       -- Focus master
       , ("M-S-m"  , windows W.focusMaster)
       -- Search a window and focus into the window
       , ("M-g"    , windowPromptGoto myXPConfig)
       -- Search a window and bring to the current workspace
       , ("M-b"    , windowPromptBring myXPConfig)
       -- Move the focus to next screen (multi screen)
       , ("M-<Tab>", nextScreen)
       -- screen
       , ("M-o"    , swapNextScreen)
       , ("M-S-o"  , shiftNextScreen)
       ]

       -------------------------------------------------------------------- }}}
       -- Keymap: Manage workspace                                          {{{
       ------------------------------------------------------------------------
       -- mod-[1..9]          Switch to workspace N
       -- mod-shift-[1..9]    Move window to workspace N

       `additionalKeys`
       [ ((m .|. myModMask, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 ..]
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

       -------------------------------------------------------------------- }}}
       -- Keymap: custom commands                                           {{{
       ------------------------------------------------------------------------

       `additionalKeysP`
       [
       -- Launch terminal
       ("M-<Return>"     , spawn myTerminal)
       -- Launch text editor
       , ("M-S-<Return>" , spawn myTextEditor)
       -- Kill window
       , ("M-C-x"        , spawn "xkill")
       -- Lock screen
       , ("M-z"          , spawn "blurlock")
       -- Reboot
       , ("M-0"          , spawn "i3exit reboot")
       -- Shutdown
       , ("M-S-0"        , spawn "i3exit shutdown")
       -- Exit
       , ("M-S-z"        , io (exitWith ExitSuccess))
       -- Restart xmonad
       , ("M-S-r",
          spawn "xmonad --recompile && xmonad --restart && notify-send 'Xmonad restarted' || notify-send 'Xmonad failed to restart'" )
       -- Launch web browser
       , ("M-<F2>"       , spawn myBrowser)
       -- Launch file manager
       , ("M-<F3>"       , spawn myFileManager)
       -- Launch Console File Manager
       , ("M-<F4>"       , spawn myConsoleFileManager)
       -- Launch dmenu for launching applicatiton
       , ("M-d"          , spawn myLauncher)
       -- Scratchpads
       , ("M-C-t"        , namedScratchpadAction scratchpads "htop")
       , ("M-C-c"        , namedScratchpadAction scratchpads "cmus")
       -- Play / Pause media keys
       , ("<XF86AudioPlay>"  , spawn "mpc toggle")
       , ("<XF86HomePage>"   , spawn "mpc toggle")
       , ("S-<F6>"           , spawn "mpc toggle")
       , ("S-<XF86AudioPlay>", spawn "streamradio pause")
       , ("S-<XF86HomePage>" , spawn "streamradio pause")
       -- Volume setting media keys
       , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
       , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
       , ("<XF86AudioMute>"       , spawn "amixer -q set Master toggle")
        -- Brightness Keys
       , ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1; notify-send 'brightness up $(xbacklight -get)")
       , ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1; notify-send 'brightness down $(xbacklight -get)")
       -- Touch pad
       , ("<XF86TouchpanOn", spawn "synclient TouchpadOff=0 && notify-send 'Touchpad On")
       , ("<XF86TouchpanOff", spawn "synclient TouchpadOff=1 && notify-send 'Touchpad Off")
       -- Explorer
       , ("<XF86Explorer", spawn myBrowser)
       -- Search
       , ("<XF86Search", spawn (myBrowser ++ " https://duckduckgo.com"))
       -- Suspendre
       , ("<XF86Suspend", spawn "i3exit suspend")
       -- Take a screenshot (whole desktop)
       , ("<Print>", spawn (myScreenCapture ++ "; notify-send 'Desktop captured'"))
       -- Take a screenshot (selected area) -- TODO fix capture area
       , ("S-<Print>", spawn (myScreenCapture ++ " -s ; notify-send 'Area captured'"))
       -- Take a screenshot (focused window)
       , ("C-<Print>", spawn (myScreenCapture ++ " -u; notify-send 'Focused window captured'"))
       ]

-- Capture Screen
myScreenCapture = "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/'"

-- DefaultTerminal
myTerminal = "termite -e tmux"

-- Launcher
myLauncher = "rofi -show drun"

-- Editor
myTextEditor = "emacsclient -c -a emacs"

-- Browser
myBrowser = "google-chrome-stable"

-- File Manager
myFileManager = "thunar"

-- Console File Manager
myConsoleFileManager = "termite -e ranger"

-- border width
myBorderWidth = 3
-- Float window control width
moveWD = 3
resizeWD = 3


defaults = docks $ desktopConfig
  {
    borderWidth        = myBorderWidth
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , focusFollowsMouse  = myFocusFollowsMouse
  , modMask            = myModMask
  , terminal           = myTerminal
  , workspaces         = myWorkspaces

  -- key bindings
  , mouseBindings      = myMouseBindings

  -- hooks
  , manageHook         = myNewManageHook
  , layoutHook         = avoidStruts $ smartBorders $ smartSpacingWithEdge 8 $ myLayout
  , startupHook        = myStartupHook
  , handleEventHook    = myHandleEventHook
  }

myWorkspaces = [
  "<fc=#78da59>\xf1d0</fc>" -- 
  , "<fc=#ffff33>\xe737</fc>" -- 
  , "<fc=#cc00ff>\xf1d1</fc>" -- 
  , "<fc=#00a1f1>\xf09b</fc>" -- 
  , "<fc=#f65314>\xe62b</fc>" -- 
  , "<fc=#f7786b>\xf79f</fc>" -- 
  , "<fc=#fbbc05>\xf197</fc>" -- 
  , "<fc=#00ffff>\xf21b</fc>" -- 
  , "<fc=#33bdf5>\xf259</fc>" -- 
  ]

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'M-S-r') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $
  myTile   |||
  myFull   |||
  mySpiral |||
  my3cmi   |||
  myMirror |||
  myTabbed
  where
    myTile = Tall 1 (3/100) (4/7)
    myFull = spacing 0 $ noBorders Full
    myMirror = Mirror (Tall 1 (3/100) (1/2))
    my3cmi =  ThreeColMid 1 (3/100) (1/2)
    myTabbed = tabbed shrinkText tabConfig
    mySpiral = spiral (6/7)


-- Scratchpads
scratchpads =
  [ NS "htop" "termite -t process -e htop" (title =? "process")  defaultFloating
  , NS "cmus" "termite -c cmus -e cmus"    (className =? "cmus") defaultFloating
  ]

-- myManageHook
myManageHook = composeAll . concat $
  [
    [className =? c --> doFloat                     | c <- myClassFloats]
  , [title     =? t --> doFloat                     | t <- myTitleFloats]
  , [className =? c --> doCenterFloat               | c <- myCenterFloats]
  , [className =? c --> doShift (myWorkspaces !! w) | (c, w) <- myShifts]
  ] where
       myCenterFloats = ["zenity"]
       myClassFloats = []
       myTitleFloats = []
       myShifts = [("keepassxc", 7), ("telegram-desktop", 5)]


myNewManageHook = composeAll
  [ myManageHook
  , floatNextHook
  , manageHook desktopConfig
  , namedScratchpadManageHook scratchpads
  ]



myStartupHook = do
  spawnOnce "stalonetray"
  spawnOnce "volumeicon"
  spawnOnce "sh -c 'sleep 40; exec keepassxc'"
  spawnOnce "sh -c 'sleep 50; exec megasync'"
  spawnOnce "sh -c 'sleep 60; exec telegram-desktop'"


myHandleEventHook = docksEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- Color Setting
colorBlue      = "#868bae"
colorGreen     = "#00d700"
colorRed       = "#ff005f"
colorGray      = "#666666"
colorWhite     = "#bdbdbd"
colorNormalbg  = "#1c1c1c"
colorfg        = "#a8b6b8"

-- Border Styling
myNormalBorderColor = "#00002c"

myFocusedBorderColor = "#4ec2f7"

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Prompt configuration
myXPConfig = defaultXPConfig
                { font              = "xft:SauceCodePro Nerd Font:size=14:antialias=true"
                , fgColor           = "#ffffff"
                , bgColor           = "#00002A"
                , borderColor       = colorNormalbg
                , height            = 30
                , promptBorderWidth = 0
                , autoComplete      = Just 100000
                , bgHLight          = "#4ec2f7"
                , fgHLight          = "#00152b"
                , position          = Top
                }

tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}
