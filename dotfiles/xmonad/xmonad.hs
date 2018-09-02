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

-- utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

import Graphics.X11.ExtraTypes.XF86
import System.IO
import Data.Bits ((.|.))
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map as M


main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
  xmonad $ defaults {
    manageHook = manageDocks <+> manageHook defaultConfig,
    -- layoutHook = avoidStruts $ layoutHook defaultConfig,
    logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "green" "" . shorten 50
                        }
  }

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

-- Border Styling
myBorderWidth = 1

myNormalBorderColor = "#BFBFBF"

myFocusedBorderColor = "#CAA9FA"

defaults = docks $ desktopConfig
  {
    borderWidth        = myBorderWidth
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , focusFollowsMouse  = myFocusFollowsMouse
  , modMask            = myModMask
  , terminal           = myTerminal
  -- , workspaces         = myWorkspaces

  -- key bindings
  , keys               = myKeys
  , mouseBindings      = myMouseBindings

  -- hooks
  , manageHook         = myNewManageHook
  , layoutHook = avoidStruts $ smartBorders $ smartSpacingWithEdge 8 $ myLayout
  -- , startupHook        = myStartupHook
  }


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
myLayout = avoidStruts $
  mySpiral |||
  myTile |||
  myFull |||
  my3cmi |||
  myMirror |||
  myTabbed
  where
    myTile = Tall 1 (3/100) (1/2)
    myFull = spacing 0 $ noBorders Full
    myMirror = Mirror (Tall 1 (3/100) (1/2))
    my3cmi =  ThreeColMid 1 (3/100) (1/2)
    myTabbed = tabbed shrinkText tabConfig
    mySpiral = spiral (6/7)

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
  [ className =? "qutebrowser"    --> doShift " Qutebrowser"
  , className =? "Spotify"        --> doShift " Media"
  , className =? "Firefox"        --> doShift " Firefox"
  , className =? "Chromium"       --> doShift " Chrome"
  , className =? "VirtualBox"     --> doShift " Misc7"
  , className =? "FocusMeNow" --> viewShift "doc"
  ]
  where viewShift = doF . liftM2 (.) W.greedyView W.shift

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
  , ((modMask, xK_b),
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

  -- Start editor
  , ((modMask, xK_p),
     spawn myTextEditor)

  -- Kill window
  , ((modMask .|. controlMask, xK_x),
     spawn "xkill")

  -- Close focused window
  , ((modMask .|. shiftMask, xK_q),
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

  -- Quit xmonad
  , ((modMask .|. shiftMask, xK_e),
     io (exitWith ExitSuccess))

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
