{-# LANGUAGE UnicodeSyntax #-}

import           System.Exit
import           XMonad                              hiding ((|||))
import           XMonad.Config.Desktop

-- hooks
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FloatNext
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doFullFloat, isDialog,
                                                      transience')
import           XMonad.Hooks.UrgencyHook

import           XMonad.Layout.Circle
import           XMonad.Layout.HintedGrid

-- layouts
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Mosaic
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.OneBig
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowNavigation

import qualified XMonad.Util.Cursor                  as Cursor

-- utils
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.Run                     (spawnPipe)
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties
import           XMonad.Util.WorkspaceCompare
import           XMonad.Util.XSelection

-- prompt
import qualified XMonad.Prompt                       as Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window

import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWS

-- actions
import           XMonad.Actions.CycleWindows
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Search
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer
import qualified XMonad.Actions.WorkspaceNames       as WorkspaceNames

import           Graphics.X11.ExtraTypes.XF86

-- Keys
import           XMonad.Util.EZConfig                as EZConfig

import           Control.Monad
import           Data.List                           (isInfixOf)
import qualified Data.Map                            as M
import           System.IO
import qualified XMonad.StackSet                     as W

main :: IO ()
main = do
  xmobar <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
  xmonad $ -- withUrgencyHook LibNotifyUrgencyHook
    --docks $
    ewmh $ myConfig {logHook = myLogHook xmobar}

myConfig =
  def
    { borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , focusFollowsMouse = myFocusFollowsMouse
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , mouseBindings = myMouseBindings
    , keys = myKeys
    , manageHook = myManageHook
    , layoutHook = myLayout
    , startupHook = myStartupHook
    , handleEventHook = myHandleEventHook
    }

myXmobarPP :: Handle -> PP
myXmobarPP h =
  xmobarPP
    { ppOutput = hPutStrLn h . \s -> " " ++ s
    , ppCurrent = xmobarColor colorNormalbg "" . wrap "(" ")"
    , ppVisible = xmobarColor colorNormalbg "" . wrap "[" "]"
    , ppUrgent =
        xmobarColor colorNormalbg colorRed . wrap "*" "*" . myXmobarStrip
    , ppLayout = myPPLayout
    , ppTitle = \str -> ""
    , ppSort = fmap (. scratchpadFilterOutWorkspace) getSortByIndex
    , ppSep = " "
    , ppWsSep = " "
    }

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList $
  [ ((modMask, button1), (\w -> focus w >> windows W.swapMaster)) -- mod-button1, Raise the window to the top of the stack
  , ((modMask, button2), (\w -> focus w >> mouseMoveWindow w)) -- mod-button2, Set the window to floating mode and move by dragging
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w)) -- mod-button3, Set the window to floating mode and resize by dragging
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys config = EZConfig.mkKeymap config myKeymap

myKeymap :: [([Char], X ())]
myKeymap =
  concat
    [ myControlKeys
    , myFloatKeys
    , myLauncherKeys
    , myLayoutKeys
    , myMediaKeys
    , myMovementKeys
    , myScratchPadKeys
    , myWorkspaceKeys
    ]

myMovementKeys =
  myWindowMovementKeys ++
  myWorkspaceMovementKeys ++
  myWorkspaceMovementKeys' ++
  myScreenMovementKeys ++
  myGotoLayoutKeys

myWindowMovementKeys =
  [ ("M-<D>", windowGo D)
  , ("M-<U>", windowGo U)
  , ("M-<L>", windowGo L)
  , ("M-<R>", windowGo R)
  , ("M-j", windowGo D)
  , ("M-k", windowGo U)
  , ("M-h", windowGo L)
  , ("M-l", windowGo R)
  , ("M-S-m", windows W.focusMaster) -- Focus master
  , ("M1-<Tab>", cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab)
  ]
  where
    windowGo = sendMessage . Go

myWorkspaceMovementKeys =
  [ (prefix ++ key, func ws)
  | (prefix, func) <-
      [ ("M-", windows . W.greedyView) -- go to workspace
      , ("M-S-", windows . viewShift) -- go to workspace taking current window
      , ("M-C-", WorkspaceNames.swapWithCurrent) -- change workspace number
      , ("M-C-S-", windows . W.shift) -- send window to workspace
      ]
  , (key, ws) <- zip keys myWorkspaces
  ]
  where
    keys = fmap return $ ['1' .. '9'] ++ ['-', '=']
    viewShift = liftM2 (.) W.greedyView W.shift

myWorkspaceMovementKeys' =
  [ ("M-C-<R>", nextWS) -- Go to the next
  , ("M-C-<L>", prevWS) --  Go to previous workspace
  , ("M-C-l", nextWS) -- Go to the next
  , ("M-C-h", prevWS) --  Go to previous workspace
  ]

myScreenMovementKeys =
  [ ("M-s", nextScreen) -- Move the focus to next screen (multi screen)
  , ("M-o", swapNextScreen)
  , ("M-S-o", shiftNextScreen)
  ]

myGotoLayoutKeys =
  [ ("M-g 1", jumpToLayout "Tall")
  , ("M-g 2", jumpToLayout "HintedGrid")
  , ("M-g 3", jumpToLayout "OneBig")
  , ("M-g 4", jumpToLayout "Circle")
  , ("M-g 5", jumpToLayout "Mosaic")
  , ("M-g 6", jumpToLayout "ThreeCol")
  , ("M-g 7", jumpToLayout "Spiral")
  ]
  where
    jumpToLayout = sendMessage . JumpToLayout

myLayoutKeys = myLayoutKeys' ++ myLayoutSwapKeys ++ myLayoutTransformKeys

myLayoutSwapKeys =
  [ ("M-S-<D>", layoutSwap D)
  , ("M-S-<U>", layoutSwap U)
  , ("M-S-<L>", layoutSwap L)
  , ("M-S-<R>", layoutSwap R)
  , ("M-S-j", layoutSwap D)
  , ("M-S-k", layoutSwap U)
  , ("M-S-h", layoutSwap L)
  , ("M-S-l", layoutSwap R)
  ]
  where
    layoutSwap = sendMessage . Swap

myLayoutKeys' =
  [ ("M-f", sendMessage $ Toggle NBFULL) -- Toggle Fullscreen mode
  , ("M-C-,", sendMessage $ IncMasterN (-1)) -- Decrease the number of master pane
  , ("M-C-.", sendMessage $ IncMasterN 1) -- Increase the number of master pane
  , ("M-<Space>", sendMessage NextLayout) -- Rotate through the available layout algorithms
  , ("M-m", windows W.shiftMaster) -- Shift the focused window to the master window
  ]

myLayoutTransformKeys =
  [ ("M-,", sendMessage Shrink)
  , ("M-.", sendMessage Expand)
  , ("M-S-.", sendMessage MirrorShrink)
  , ("M-S-,", sendMessage MirrorExpand)
  , ("M-g x", sendMessage $ Toggle REFLECTX)
  , ("M-g y", sendMessage $ Toggle REFLECTY)
  , ("M-g m", sendMessage $ Toggle MIRROR) -- Toggle Mirror layout
  ]

myWorkspaceKeys =
  [ ("M-C-S-<R>", shiftToNext) -- Shift the focused window to the next workspace
  , ("M-C-S-<L>", shiftToPrev) -- Shift the focused window to the previous workspace
  , ("M-C-S-l", shiftToNext) -- Shift the focused window to the next workspace
  , ("M-C-S-h", shiftToPrev) -- Shift the focused window to the previous workspace
  , ("M-<Tab>", toggleWS) -- toggle last workspace
  ]

myFloatKeys =
  [ ("M-t s", withFocused $ windows . W.sink)
  , ("M-t b", withFocused $ windows . flip W.float bigCenterR)
  , ("M-t c", withFocused $ windows . flip W.float centerR)
  , ("M-t l", withFocused $ windows . flip W.float leftR)
  , ("M-t r", withFocused $ windows . flip W.float rightR)
  ]

myLauncherKeys = myLauncherKeys' ++ myScreenCaptureKeys

myLauncherKeys' =
  fmap spawn <$>
  [ ("M-<Return>", myTerminal) -- Launch terminal
  , ("M-e b", myBrowser) -- Launch text editor
  , ("M-e e", myTextEditor) -- Launch text editor
  , ("M-e f", myFileManager) -- Launch text editor
  , ("M-e k", "xkill") -- Kill window
  , ("M-e r", myConsoleFileManager) -- Launch text editor
  , ("M-e t", myTmuxTerminal) -- Launch tmux terminal
  , ("M-e v", "nvim") -- Launch text editor
  , ("M-S-C-=", "$HOME/.scripts/xbacklight-toggle.sh")
  ]

myScreenCaptureKeys =
  fmap spawn <$>
  [ ( "<Print>" -- Take a screenshot (whole desktop)
    , myScreenCapture ++ " && notify-send 'Desktop captured'")
  , ( "S-<Print>" -- Take a screenshot (selected area)
    , "notify-send 'Select Area';sleep 0.2;" ++
      myScreenCapture ++ " -s && notify-send 'Area captured'")
  , ( "C-<Print>" -- Take a screenshot (focused window)
    , myScreenCapture ++ " -u && notify-send 'Focused window captured'")
  ]

myMediaKeys =
  fmap spawn <$>
    -- Play / Pause media
  [ ("<XF86AudioPlay>", "playerctl play-pause")
  , ("<XF86AudioStop>", "playerctl pause")
  , ("<XF86AudioPrev>", "playerctl previous")
  , ("<XF86AudioNext>", "playerctl next")
  , ("<XF86HomePage>", "mpc toggle")
  -- Volume
  , ("<XF86AudioRaiseVolume>", "$HOME/.scripts/VolControl.sh up")
  , ("<XF86AudioLowerVolume>", "$HOME/.scripts/VolControl.sh down")
  , ("<XF86AudioMute>", "$HOME/.scripts/XMMute.sh")
  -- Brightness
  , ( "<XF86MonBrightnessUp>"
    , "xbacklight + 5 -time 100 -steps 1 && notify-send \"brightness up $(xbacklight -get)\"")
  , ( "<XF86MonBrightnessDown>"
    , "xbacklight - 5 -time 100 -steps 1 && notify-send \"brightness down $(xbacklight -get)\"")
  -- Touchpad
  , ("<XF86TouchpadToggle>", "$HOME/.scripts/touchpad_toggle.sh") -- Touch pad
  -- Browser
  , ("<XF86Explorer>", myBrowser) -- Browser
  , ("<XF86Search>", myBrowser ++ " https://duckduckgo.com") -- Search
  ]

myScratchPadKeys =
  [ ("M-n h", namedScratchpadAction scratchpads "htop")
  , ("M-n c", namedScratchpadAction scratchpads "cmus")
  ]

myControlKeys = myXmonadKeys ++ myLogoutKeys

myXmonadKeys =
  [ ("M-S-q", kill) -- Close the focused window
       -- Toggle struts
  , ("M-b", sendMessage ToggleStruts)
       -- grid selection
  , ("M-g s", goToSelected myGridSelectConfig)
       -- Search a window and focus into the window
  , ("M-g g", windowPrompt myPromptInfix Goto allWindows)
       -- Search a window and bring to the current workspace
  , ("M-g b", windowPrompt myPromptInfix Bring allWindows)
  , ("M-g l", myLayoutPrompt)
       -- Resize viewed windows to the correct size
  --, ("M-n", refresh)
    --  Reset the layouts on the current workspace to default
    --, ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
  , ( "M-S-r" -- Restart xmonad
    , spawn
        "xmonad --recompile && xmonad --restart && notify-send 'Xmonad restarted' || notify-send 'Xmonad failed to restart'")
  , ("M-r", spawn "xmonad --restart") -- restart xmonad w/o recompiling
  , ("M-d", shellPrompt myPrompt) -- launch apps
  , ("M-S-d", spawn myLauncher) -- launch apps
  , ("M-?", helpCommand)
  ]

myLogoutKeys =
  [ ("M-; l", myLogoutPrompt "lock") -- Lock screen
  , ("M-; s", myLogoutPrompt "suspend") -- suspend
  , ("M-; r", myLogoutPrompt "reboot") -- reboot
  , ("M-; h", myLogoutPrompt "shutdown") -- shutdown // halt
  , ("M-; e", confirmPrompt myPrompt "Exit" $ io (exitWith ExitSuccess)) -- Exit
  ]

------------------------------------------------------------------------
-- Default Apps
--
-- Capture Screen
myScreenCapture :: String
myScreenCapture = "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/'"

myTerminal :: String
myTerminal = "alacritty"

myTmuxTerminal :: String
myTmuxTerminal = myTerminal ++ " -e tmux attach"

-- Launcher
myLauncher :: String
myLauncher = "rofi -show drun"

-- Editor
myTextEditor :: String
myTextEditor = "emacsclient -c -a emacs"

-- Browser
myBrowser :: String
myBrowser = "firefox"

-- File Manager
myFileManager :: String
myFileManager = "thunar"

-- Console File Manager
myConsoleFileManager :: String
myConsoleFileManager = myTerminal ++ " -e ranger"

-- myLayout
myPPLayout :: [Char] -> [Char]
myPPLayout =
  (\x ->
     case x of
       "Tall"            -> "\xf005" -- 
       "ThreeCol"        -> "\xfa6a" -- 頻
       "Mirror ThreeCol" -> "\xfa6e" -- 﩮
       "Spiral"          -> "\xf306" -- 
       "Mosaic"          -> "\xfa6d" -- 舘
       "Full"            -> "\xf5b5" -- 
       "Mirror Tall"     -> "\xf006" -- 
       "Mirror Mosaic"   -> "\xfa73" -- 侀
       "Tabbed"          -> "\xfd35" -- ﴵ
       "Mirror Spiral"   -> "\xfc06" -- ﰆ
       "Circle"          -> "\xe22e" -- 
       "OneBig"          -> "\xf286" -- 
       "HintedGrid"      -> "\xfb8a" -- ﮊ
       _                 -> x)

myWorkspaces :: [String]
myWorkspaces =
  fmap myWorkspaceIcon $
  [ (colorConiferGreen, "\xf1d0") -- 
  , (colorGoldenFizzYellow, "\xe737") -- 
  , (colorElectricViolet, "\xf1d1") -- 
  , (colorCeruleanBlue, "\xf09b") -- 
  , (colorPomegranate, "\xe62b") -- 
  , (colorFrolyPink, "\xf79f") -- 
  , (colorSelectiveYellow, "\xf197") -- 
  , (colorCyan, "\xf21b") -- 
  , (colorPictonBlue, "\xf259") -- 
  ]
  where
    myWorkspaceIcon (color, icon) = "<fc=" ++ color ++ ">" ++ icon ++ "</fc>"

------------------------------------------------------------------------
-- Layouts
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'M-S-r') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
  avoidStruts $
  -- Toggles
  mkToggle1 NBFULL $
  mkToggle1 REFLECTX $
  mkToggle1 REFLECTY $
  mkToggle1 MIRROR $
  configurableNavigation (navigateColor myNormalBorderColor) $
  -- Layouts
  name "Tall" myTile |||
  name "HintedGrid" myHintedGrid |||
  name "OneBig" myOneBig |||
  name "Circle" Circle |||
  name "Mosaic" myMosaic ||| name "ThreeCol" my3cmi ||| name "Spiral" mySpiral
  where
    name n = renamed [Replace n] . spacing 5
    myTile = ResizableTall 1 (3 / 100) (4 / 7) []
    my3cmi = ThreeColMid 1 (3 / 100) (1 / 2)
    mySpiral = spiral (6 / 7)
    myMosaic = mosaic 2 [3, 2]
    myHintedGrid = GridRatio (4 / 3) False
    myOneBig = OneBig (4 / 6) (4 / 6)

------------------------------------------------------------------------
-- Scratchpads
--
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "htop"
      (myTerminal ++ " -t process -e htop")
      (title =? "process")
      myPosition
  , NS
      "cmus"
      (myTerminal ++ " -t process -e cmus")
      (resource =? "cmus")
      myPosition
  ]
  where
    myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)

------------------------------------------------------------------------
-- Manage Hooks
--
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ manageDocks
    , floatNextHook
    , manageHook desktopConfig
    , namedScratchpadManageHook scratchpads
    , myManageHook'
    ]

myLogHook :: Handle -> X ()
myLogHook b
          -- ewmhDesktopsLogHook >> (dynamicLogWithPP $ myXmobarPP xmobar)
 = (dynamicLogWithPP $ myXmobarPP b) >> updatePointer (0.5, 0.5) (1, 1)

--
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions
-- xprop fields used in manage hook:
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
-- https://hackage.haskell.org/package/xmonad-0.15/docs/XMonad-ManageHook.html
myManageHook' =
  composeAll . concat $
  [ [isDialog --> doCenterFloat]
  , [className =? c --> doFloat | c <- myClassFloats]
  , [className =? c --> doFullFloat | c <- myFullFloats]
  , [title =? t --> doFloat | t <- myTitleFloats]
  , [className =? c --> doCenterFloat | c <- myCenterFloats]
  , [title =? t --> doCenterFloat | t <- myTitleCenterFloats]
  , [className =? c --> doShift (myWorkspaces !! ws) | (c, ws) <- myShifts]
  , [transience'] -- move transient windows like dialogs/alerts on top of their parents
  ]
  where
    myCenterFloats = ["zenity", "Arandr", "Galculator", "Yad"]
    myTitleCenterFloats = ["File Operation Progress", "Downloads", "Save as..."]
    myClassFloats = []
    myTitleFloats = ["Media viewer", "Yad"]
    myFullFloats = []
       -- workspace numbers start at 0
    myShifts =
      [ ("keepassxc", 6)
      , ("telegram-desktop", 4)
      , ("TelegramDesktop", 4)
      , ("Thunderbird", 4)
      , ("Slack", 5)
      , ("Spotify", 6)
      , ("vivaldi-stable", 0)
      ]

myStartupHook :: X ()
myStartupHook = do
  EZConfig.checkKeymap myConfig myKeymap
  Cursor.setDefaultCursor Cursor.xC_left_ptr
  spawn "$HOME/.scripts/autostart.sh"

myHandleEventHook =
  fullscreenEventHook <+> docksEventHook <+> handleEventHook desktopConfig

data LibNotifyUrgencyHook =
  LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    ws <- gets windowset
    whenJust (W.findTag w ws) (flash name)
    where
      flash name index =
        safeSpawn
          "notify-send"
          [(show name ++ " requests your attention on workspace " ++ index)]

------------------------------------------------------------------------
-- Prompt
--
-- Prompt configuration
myPrompt :: XPConfig
myPrompt =
  def
    { Prompt.font = "xft:SauceCodePro Nerd Font:size=14:antialias=true"
    , Prompt.fgColor = colorWhite
    , Prompt.bgColor = colorPromptbg
    , Prompt.borderColor = colorPromptbg
    , Prompt.height = 30
    , Prompt.promptBorderWidth = 0
    , Prompt.autoComplete = Just 100000
    , Prompt.bgHLight = colorPromptHLightbg
    , Prompt.fgHLight = colorPromptHLightfg
    , Prompt.position = Prompt.Top
    , Prompt.maxComplRows = Just 5
    , Prompt.searchPredicate = isPrefixOf
    }

myPromptInfix :: XPConfig
myPromptInfix = myPrompt {Prompt.searchPredicate = isInfixOf}

myLayoutPrompt :: X ()
myLayoutPrompt =
  inputPromptWithCompl
    myPrompt {Prompt.autoComplete = Just 1000}
    "Layout"
    (Prompt.mkComplFunFromList'
       [ "1.Tall"
       , "2.HintedGrid"
       , "3.OneBig"
       , "4.Circle"
       , "5.Mosaic"
       , "6.ThreeCol"
       , "7.Spiral"
       ]) ?+ \l -> sendMessage $ JumpToLayout $ drop 2 l

myLogoutPrompt :: String -> X ()
myLogoutPrompt command =
  confirmPrompt myPrompt command $
  spawn ("$HOME/.scripts/i3lock.sh " ++ command)

------------------------------------------------------------------------
-- Colors
--
-- X11 color names:
-- https://www.wikiwand.com/en/X11_color_names
-- Color Setting
colorBlue :: String
colorBlue = "#868bae"

colorGreen :: String
colorGreen = "#00d700"

colorRed :: String
colorRed = "#ff005f"

colorGray :: String
colorGray = "#666666"

colorWhite :: String
colorWhite = "#bdbdbd"

colorPromptbg :: String
colorPromptbg = "#1c1c1c"

colorPromptHLightfg :: String
colorPromptHLightfg = "#00152b"

colorPromptHLightbg :: String
colorPromptHLightbg = "#4ec2f7"

colorNormalbg :: String
colorNormalbg = "#f8f8f8"

colorfg :: String
colorfg = "#a8b6b8"

-- Border Styling
myNormalBorderColor :: String
myNormalBorderColor = "#71469b"

myFocusedBorderColor :: String
myFocusedBorderColor = "#87CEFA"

-- name colors http://chir.ag/projects/name-that-color
colorConiferGreen :: String
colorConiferGreen = "#78ea59"

colorGoldenFizzYellow :: String
colorGoldenFizzYellow = "#ffff33"

colorElectricViolet :: String
colorElectricViolet = "#cc00ff"

colorCeruleanBlue :: String
colorCeruleanBlue = "#00a1f1"

colorPomegranate :: String
colorPomegranate = "#f65314"

colorFrolyPink :: String
colorFrolyPink = "#f7786b"

colorSelectiveYellow :: String
colorSelectiveYellow = "#fbbc05"

colorCyan :: String
colorCyan = "#00ffff"

colorPictonBlue :: String
colorPictonBlue = "#33bdf5"

-- border width
myBorderWidth :: Dimension
myBorderWidth = 6

------------------------------------------------------------------------
-- Floats
--
centerR = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)

bigCenterR = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

leftR = W.RationalRect (0) (1 / 8) (1 / 2) (3 / 4)

rightR = W.RationalRect (4 / 8) (1 / 8) (1 / 2) (3 / 4)

myFloats =
  cycle -- TODO cycle through the floats instead of assigning a keybinding to each one
    [ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2) -- center
    , W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4) -- bigCenter
    , W.RationalRect (0) (1 / 8) (1 / 2) (3 / 4) -- left
    , W.RationalRect (4 / 8) (1 / 8) (1 / 2) (3 / 4) -- right
    ]

-- GridSelect configuration
myGridSelectConfig :: GSConfig Window
myGridSelectConfig =
  def {gs_navigate = myNavigation, gs_colorizer = fromClassName}
  where
    myNavigation =
      makeXEventhandler $ shadowWithKeymap navKeymap $ const defaultNavigation
    navKeymap =
      M.fromList
        [ ((0, xK_Escape), cancel)
        , ((0, xK_Return), select)
        , ((0, xK_slash), substringSearch myNavigation) -- search
        , ((0, xK_h), move (-1, 0) >> myNavigation) -- move left
        , ((0, xK_l), move (1, 0) >> myNavigation) -- move right
        , ((0, xK_j), move (0, 1) >> myNavigation) -- move down
        , ((0, xK_k), move (0, -1) >> myNavigation) -- move up
        , ((0, xK_y), move (-1, -1) >> myNavigation) -- move diagonal up left
        , ((0, xK_u), move (1, -1) >> myNavigation) -- move diagonal up right
        , ((0, xK_b), move (-1, 1) >> myNavigation) -- move diagonal down left
        , ((0, xK_n), move (1, 1) >> myNavigation) -- move diagonal down right
        , ((0, xK_Tab), moveNext >> myNavigation) -- move next
        ]

------------------------------------------------------------------------
-- Testing
--
-- https://snipt.net/doitian/xmonad-configuration/
helpCommand :: X ()
helpCommand = spawn ("echo " ++ show help ++ " | xmessage -file -")

help :: String
help =
  unlines
    [ "The modifier key is 'Super'. keybindings:"
    , ""
    , "-- launching and killing programs"
    , ""
    , "-- move focus up or down the window stack"
    , ""
    , "-- modifying the window order"
    , ""
    , "-- resizing the master/slave ratio"
    , ""
    , "-- floating layer support"
    , ""
    , "-- increase or decrease number of windows in the master area"
    , ""
    , "-- Workspaces & screens"
    , "mod-[1..9]         Switch to workSpace N"
    , "mod-Shift-[1..9]   Move client to workspace N"
    , ""
    , "-- Mouse bindings: default actions bound to mouse events"
    , "mod-button1  Set the window to floating mode and move by dragging"
    , "mod-button2  Raise the window to the top of the stack"
    , "mod-button3  Set the window to floating mode and resize by dragging"
    ]

delicious = searchEngine "delicious" "http://delicious.com/doitian/"

searchEngineMap method =
  M.fromList $
  [ ((0, xK_b), method delicious)
  , ((0, xK_d), method dictionary)
  , ((0, xK_g), method google)
  , ((0, xK_w), method wikipedia)
  ]

myXmobarStrip :: String -> String
myXmobarStrip = strip []
  where
    strip keep x
      | null x = keep
      | "<fc=" `isPrefixOf` x = strip keep (drop 1 . dropWhile (/= '>') $ x)
      | "</fc>" `isPrefixOf` x = strip keep (drop 5 x)
      | '<' == head x = strip (keep ++ "<") (tail x)
      | otherwise =
        let (good, x') = span (/= '<') x
         in strip (keep ++ good) x'
-- TODO remove markdown from prompt to bring/go to window
--myWindows :: XWindowMap
--myWindows = withWindowSet (return . W.index) >>= winMap
--  where
--    winMap = fmap M.fromList . mapM pair
--    pair w = do
--      name <- fmap show $ getName w
--      return (myXmobarStrip name, w)
--windowMap' :: (X.WindowSpace -> Window -> X String) -> X (M.Map String Window)
--windowMap' titler = do
--  ws <- gets X.windowset
--  M.fromList . concat <$> mapM keyValuePairs (W.workspaces ws)
-- where keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
--       keyValuePair ws w = flip (,) w <$> titler ws w
--
--
---- | Returns the window name as will be listed in dmenu.
----   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
----   know where he's going.
--mdecorateName :: X.WindowSpace -> Window -> X String
--mdecorateName ws w = do
--  name <- show <$> getName w
--  return $ name ++ " [" ++ W.tag ws ++ "]"
--
--myWindows :: XWindowMap
--myWindows = windowMap' mDecorateName
--
--myDecorateName :: X.WindowSpace -> Window -> X String
--myDecorateName ws w = do
--  name <- show <$> getName w
--  return $ name ++ " [2" ++ W.tag ws ++ "2]"
