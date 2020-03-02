{-# LANGUAGE UnicodeSyntax #-}

--
-- If there are more than 3 explicit imports required a qualified import is used
-- If a type has only one constructor it is imported implicitly with (..)
--
--
import           XMonad                              hiding ((|||))

-- hooks
import qualified XMonad.Hooks.DynamicLog             as DL
import           XMonad.Hooks.EwmhDesktops           (ewmh)
import           XMonad.Hooks.FloatNext              (floatNextHook)
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import qualified XMonad.Hooks.ManageHelpers          as ManageHelpers
import qualified XMonad.Hooks.UrgencyHook            as UH

-- layouts
import           XMonad.Layout.Circle                (Circle (..))
import           XMonad.Layout.HintedGrid            (Grid (GridRatio))
import           XMonad.Layout.LayoutCombinators     (JumpToLayout (JumpToLayout), (|||))
import           XMonad.Layout.Mosaic                (mosaic)
import           XMonad.Layout.MultiToggle           (Toggle (..), mkToggle1)
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL))
import           XMonad.Layout.OneBig                (OneBig (OneBig))
import           XMonad.Layout.Reflect               (REFLECTX (..), REFLECTY (..))
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import qualified XMonad.Layout.ResizableTile         as RTile
import           XMonad.Layout.Spacing               (spacing)
import           XMonad.Layout.Spiral                (spiral)
import           XMonad.Layout.ThreeColumns          (ThreeCol (ThreeColMid))
import qualified XMonad.Layout.WindowNavigation      as Nav

-- utils
import qualified XMonad.Util.Cursor                  as Cursor
import           XMonad.Util.EZConfig                (checkKeymap, mkKeymap)
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.Run                     (safeSpawn, spawnPipe)
import           XMonad.Util.Scratchpad              (scratchpadFilterOutWorkspace)
import           XMonad.Util.WorkspaceCompare        (getSortByIndex)
import           XMonad.Util.Ungrab                  (unGrab)

-- prompt
import qualified XMonad.Prompt                       as Prompt
import           XMonad.Prompt.ConfirmPrompt         (confirmPrompt)
import           XMonad.Prompt.Input                 (inputPromptWithCompl, (?+))
import           XMonad.Prompt.Shell                 (shellPrompt)
import qualified XMonad.Prompt.Window                as WPrompt

-- actions
import qualified XMonad.Actions.CycleWS              as CycleWS
import           XMonad.Actions.CycleWindows         (cycleRecentWindows)
import qualified XMonad.Actions.GridSelect           as GS
import qualified XMonad.Actions.Search               as Search
import           XMonad.Actions.UpdatePointer        (updatePointer)
import           XMonad.Actions.WorkspaceNames       (swapWithCurrent)
import           XMonad.Actions.Submap               (submap)

import           XMonad.Config.Desktop               (desktopConfig)
import qualified XMonad.StackSet                     as W

import           Control.Monad                       (liftM2)
import qualified Data.List                           as L
import           Data.List.Split                     (chunksOf)
import qualified Data.Map                            as M
import qualified Data.Text                           as T
import           Data.Char                           (toLower)
import           System.Exit                         (ExitCode (ExitSuccess), exitWith)
import           System.IO                           (Handle, hPutStrLn, hClose)

------------------------------------------------------------------------
-- Main
--
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
  xmonad $
    ewmh $ UH.withUrgencyHook UH.NoUrgencyHook $ myConfig {logHook = myLogHook xmproc}

------------------------------------------------------------------------
-- Config
--
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

------------------------------------------------------------------------
-- dzen
--
myDzenPP :: Handle -> DL.PP
myDzenPP h =
  DL.defaultPP
    { DL.ppOutput = hPutStrLn h . \s -> " " ++ s
    , DL.ppCurrent = DL.xmobarColor lightWhite "" . DL.wrap "(" ")"
    , DL.ppVisible = DL.xmobarColor lightWhite "" . DL.wrap "[" "]"
    , DL.ppUrgent =
        DL.xmobarColor colorRed ""
    , DL.ppLayout = myPPLayout
    , DL.ppTitle = DL.shorten 30 . DL.wrap " " " "
    , DL.ppSort = fmap (. scratchpadFilterOutWorkspace) getSortByIndex
    , DL.ppSep = " "
    , DL.ppWsSep = " "
    }

------------------------------------------------------------------------
-- xmobar
--
myXmobarPP :: Handle -> DL.PP
myXmobarPP h =
  DL.xmobarPP
    { DL.ppOutput = hPutStrLn h . \s -> " " ++ s
    , DL.ppCurrent = DL.xmobarColor lightWhite "" . DL.wrap "(" ")"
    , DL.ppVisible = DL.xmobarColor lightWhite "" . DL.wrap "[" "]"
    , DL.ppUrgent =
        DL.xmobarColor colorRed ""
    , DL.ppLayout = myPPLayout
    , DL.ppTitle = DL.shorten 30 . DL.wrap " " " "
    , DL.ppSort = fmap (. scratchpadFilterOutWorkspace) getSortByIndex
    , DL.ppSep = " "
    , DL.ppWsSep = " "
    }

------------------------------------------------------------------------
-- Default Apps
--
-- Capture Screen
myScreenCapture :: String
myScreenCapture = "$HOME/.scripts/screen_shot.sh"

myTerminal :: String
myTerminal = "xfce4-terminal"

myTmuxTerminal :: String
myTmuxTerminal = myTerminal ++ " -e tmux attach"

-- Launcher
myLauncher :: String
myLauncher = "rofi -show drun -no-plugins"

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
myPPLayout :: String -> String
myPPLayout x =
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
       _                 -> x

myWorkspaces :: [String]
myWorkspaces =
  [  "\xE907" -- 
  ,  "\xE905" -- 
  ,  "\xE92D" -- 
  ,  "\xE948" -- 
  ,  "\xE929" -- 
  ,  "\xE93E" -- 
  ,  "\xE926" -- 
  ,  "\xE932" -- 
  ,  "\xE97D" -- 
  ,  "\xE982" -- 
  ]

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
  ManageDocks.avoidStruts $
  -- Toggles
  mkToggle1 NBFULL $
  mkToggle1 REFLECTX $
  mkToggle1 REFLECTY $
  mkToggle1 MIRROR $
  Nav.configurableNavigation (Nav.navigateColor myNormalBorderColor) $
  -- Layouts
  name "Tall" myTile |||
  name "HintedGrid" myHintedGrid |||
  name "OneBig" myOneBig |||
  name "Circle" Circle |||
  name "Mosaic" myMosaic ||| name "ThreeCol" my3cmi ||| name "Spiral" mySpiral
  where
    name n = renamed [Replace n] . spacing 2
    myTile = RTile.ResizableTall 1 (3 / 100) (4 / 7) []
    my3cmi = ThreeColMid 1 (3 / 100) (1 / 2)
    mySpiral = spiral (6 / 7)
    myMosaic = mosaic 2 [3, 2]
    myHintedGrid = GridRatio (4 / 3) False
    myOneBig = OneBig (4 / 6) (4 / 6)

------------------------------------------------------------------------
-- Manage Hooks
--
myLogHook :: Handle -> X ()
myLogHook b
 = DL.dynamicLogWithPP (myXmobarPP b) >> updatePointer (0.5, 0.5) (0, 0)

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ ManageDocks.manageDocks
    , floatNextHook
    , manageHook desktopConfig
    , myManageHook'
    ]

--
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions
-- xprop fields used in manage hook:
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
-- https://hackage.haskell.org/package/xmonad-0.15/docs/XMonad-ManageHook.html
myManageHook' =
  composeAll . concat $
    [ [ManageHelpers.transience'] -- move transient windows like dialogs/alerts on top of their parents
    , [className =? c --> doFloat | c <- myClassFloats]
    , [className =? c --> ManageHelpers.doFullFloat | c <- myFullFloats]
    , [title =? t --> doFloat | t <- myTitleFloats]
    , [className =? c --> ManageHelpers.doCenterFloat | c <- myCenterFloats]
    , [title =? t --> ManageHelpers.doCenterFloat | t <- myTitleCenterFloats]
    , [className =? c --> doShift (myWorkspaces !! ws) | (c, ws) <- myShifts]
    -- , [ManageHelpers.isDialog --> ManageHelpers.doCenterFloat]
    ]
    where
      myCenterFloats = ["zenity", "Arandr", "Galculator", "Yad", "albert"]
      myTitleCenterFloats = ["File Operation Progress", "Downloads", "Save as...", "Ulauncher Preferences"]
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
        , ("Spotify", 5)
        , ("Odio", 5)
        ]

myStartupHook :: X ()
myStartupHook = do
  checkKeymap myConfig myKeymap
  Cursor.setDefaultCursor Cursor.xC_left_ptr
  spawn "$HOME/.xmonad/autorun.sh"

myHandleEventHook =
  ManageDocks.docksEventHook <+> handleEventHook desktopConfig

------------------------------------------------------------------------
-- Prompt
--
-- Prompt configuration
myPrompt :: Prompt.XPConfig
myPrompt =
  def
    { Prompt.font = "xft:Fantasque Sans Mono Nerd Font:size=14:antialias=true"
    , Prompt.fgColor = foreground
    , Prompt.bgColor = background
    , Prompt.borderColor = colorPromptbg
    , Prompt.height = 22
    , Prompt.promptBorderWidth = 0
    , Prompt.autoComplete = Just 100000
    , Prompt.bgHLight = colorPromptHLightbg
    , Prompt.fgHLight = colorPromptHLightfg
    , Prompt.position = Prompt.Top
    , Prompt.maxComplRows = Just 5
    , Prompt.searchPredicate = L.isPrefixOf
    }

myPromptInfix :: Prompt.XPConfig
myPromptInfix = myPrompt {Prompt.searchPredicate = L.isInfixOf}

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

mySessionPrompt :: X ()
mySessionPrompt =
  inputPromptWithCompl
    myPrompt {Prompt.autoComplete = Just 1000}
    "\x23FB " -- ⏻
    (Prompt.mkComplFunFromList'
       [ "1.Lock"
       , "2.Suspend"
       , "3.Reboot"
       , "4.Shutdown"
       , "5.Exit"
       ]) ?+ \l -> prompt $ map toLower $ drop 2 l
       where 
        prompt =
              \x -> 
              case x of 
                "lock" -> noConfirm x
                "suspend" -> noConfirm x
                "reboot" -> confirm x
                "shutdown" -> confirm x
                "exit" -> confirmPrompt myPrompt x $ io (exitWith ExitSuccess) 
               where
                confirm command = confirmPrompt myPrompt command $ spawn ("$HOME/.scripts/i3lock.sh " ++ command)
                noConfirm command = spawn ("$HOME/.scripts/i3lock.sh " ++ command)

------------------------------------------------------------------------
-- Colors

background = "#1D1F28"
foreground = "#FDFDFD"
lightBlack   = "#282A36" -- color0
lightRed     = "#F37F97" -- color1
lightGreen   = "#5ADECD" -- color2
lightYellow  = "#F2A272" -- color3
lightBlue    = "#8897F4" -- color4
lightMagenta = "#C574DD" -- color5
lightCyan    = "#79E6F3" -- color6
lightWhite   = "#FDFDFD" -- color7
lightGray    = "#C0C0C0"
darkBlack   = "#414458" -- color8
darkRed     = "#FF4971" -- color9
darkGreen   = "#18E3C8" -- color10
darkYellow  = "#FF8037" -- color11
darkBlue    = "#556FFF" -- color12
darkMagenta = "#B043D1" -- color13
darkCyan    = "#3FDCEE" -- color14
darkWhite   = "#BEBEC1" -- color15
darkGray    = "#848482"
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
colorPromptHLightfg = foreground

colorPromptHLightbg :: String
colorPromptHLightbg = background

colorNormalbg :: String
colorNormalbg = background

colorfg :: String
colorfg = foreground

-- Border Styling
myNormalBorderColor :: String
myNormalBorderColor = "#C574DD"

myFocusedBorderColor :: String
myFocusedBorderColor = "#5ADECD"

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
myBorderWidth = 3

------------------------------------------------------------------------
-- Floats
--
centerR = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)

bigCenterR = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)

leftR = W.RationalRect 0 (1 / 8) (1 / 2) (3 / 4)

rightR = W.RationalRect (4 / 8) (1 / 8) (1 / 2) (3 / 4)

-- GridSelect configuration
myGridSelectConfig :: GS.GSConfig Window
myGridSelectConfig =
  def {GS.gs_navigate = myNavigation, GS.gs_colorizer = GS.fromClassName}
  where
    myNavigation =
      GS.makeXEventhandler $
      GS.shadowWithKeymap navKeymap $ const GS.defaultNavigation
    navKeymap =
      M.fromList
        [ ((0, xK_Escape), GS.cancel)
        , ((0, xK_Return), GS.select)
        , ((0, xK_slash), GS.substringSearch myNavigation) -- search
        , ((0, xK_h), GS.move (-1, 0) >> myNavigation) -- move left
        , ((0, xK_l), GS.move (1, 0) >> myNavigation) -- move right
        , ((0, xK_j), GS.move (0, 1) >> myNavigation) -- move down
        , ((0, xK_k), GS.move (0, -1) >> myNavigation) -- move up
        , ((0, xK_y), GS.move (-1, -1) >> myNavigation) -- move diagonal up left
        , ((0, xK_u), GS.move (1, -1) >> myNavigation) -- move diagonal up right
        , ((0, xK_b), GS.move (-1, 1) >> myNavigation) -- move diagonal down left
        , ((0, xK_n), GS.move (1, 1) >> myNavigation) -- move diagonal down right
        , ((0, xK_Tab), GS.moveNext >> myNavigation) -- move next
        ]


-------------------------------------------------------------------------
-- Keybinding hints
--

rmDesc :: [(a,b,c)] -> [(a,b)]
rmDesc x = [(t1,t2) | (t1,t2,_) <- x]

fmtDesc :: String -> [(String, a, String)] -> Int -> String -> String -> String
fmtDesc name map rows fg hl | name == "" = "\"" ++ "\\n" ++ list ++ "\""
                            | otherwise  = "\"" ++ colStr hl ++ name ++ "\\n\\n" ++ list ++ "\""
    where
        list = L.intercalate "\\n" (foldr (zipWithMore (++)) [""] col)
        col = chunksOf nRows $ colDesc map
        --sortKeys  = L.sortBy (\(a,_,_) (b,_,_) -> compare a b)
        maxChars = 220
        lMap = length map
        nRows = min rows lMap
        nCol = max 1 $ ceiling $ fromIntegral lMap / fromIntegral nRows
        charsPerCol = quot maxChars nCol
        charsPerICol = quot charsPerCol 2

        descAlign = charsPerICol
        keyAlign = charsPerICol
  
        colDesc :: [(String, a, String)] -> [String]
        colDesc x = [ colStr hl ++ rAlign keyAlign key ++ " " ++ colStr fg ++ lAlign descAlign desc | (key,_,desc) <- x]
  
        colStr :: String -> String
        colStr col = "^fg(" ++ col ++ ")" 
  
        rAlign :: Int -> String -> String
        rAlign = textAlign T.justifyRight
  
        lAlign :: Int -> String -> String
        lAlign = textAlign T.justifyLeft

        textAlign :: (Int -> Char -> T.Text -> T.Text) -> Int -> (String -> String)
        textAlign fAlign n = T.unpack . fAlign n ' ' . T.pack
  
        zipWithMore :: (a -> a -> a) -> [a] -> [a] -> [a]
        zipWithMore f (a:as) (b:bs) = f a b : zipWithMore f as bs
        zipWithMore _ []      bs    = bs -- if there's more in bs, use that
        zipWithMore _ as      []    = as -- if there's more in as, use that

inputDoc :: String -> String -> String -> X Handle
inputDoc name fg bg = 
  -- focused screen location/size
    spawnPipe $ unwords [ "$HOME/.scripts/showHintForInputMode.sh"
                                  , show name
                                  , show fg
                                  , show bg
                                  , "22"
                                  ]

keyMapDoc :: String -> String -> String -> Int -> X Handle
keyMapDoc desc id color delay = 
  -- focused screen location/size
    spawnPipe $ unwords [ "$HOME/.scripts/showHintForKeymap.sh"
                                  , desc
                                  , id
                                  , "22"
                                  , show delay
                                  , show color
                                  , show 0
                                  ]


toSubmapP :: XConfig l -> String -> [(String, X (), String)] -> X ()
toSubmapP c name map = do
  p1 <- inputDoc name lightCyan lightRed
  p2 <- keyMapDoc desc "dzen_xmonad_p" background 1
  inputMode p2 $ getKeymap c keyMap
  io $ hClose p1
        where
            desc = fmtDesc "" map 4 lightCyan lightRed
            keyMap = rmDesc map
            getKeymap c map = M.toList (mkKeymap c map)

toSubmap :: XConfig l -> String -> [(String, X (), String)] -> X ()
toSubmap c name map = do
    pipe <- keyMapDoc desc "dzen_xmonad" background 1
    submap $ mkKeymap c keyMap
    io $ hClose pipe
        where
            desc = fmtDesc name map 5 lightBlue lightYellow
            keyMap = rmDesc map

-- | Given a list of key bindings, return an action that temporay modifies
--   your bindings. Hit `Escape` to switch back to normal key bindings.
inputMode :: Handle -> [((KeyMask, KeySym), X ())] -> X ()
inputMode handle bindings = submap modeMap
  where 
      modeMap = M.fromList
                $ ((0, xK_Escape), inputModeAction handle $ return ())
                : [ (maskedKey, inputModeAction handle action >> submap modeMap)
                  | (maskedKey, action) <- bindings
                  ]
                      where 
                          inputModeAction handle action = do
                              io $ hClose handle
                              action

showHelp :: X ()
showHelp = spawn $ unwords [ "$HOME/.scripts/showHintForKeymap.sh"
                    ,desc
                    , "dzen_xmonad"
                    , "22" 
                    , "0"
                    , show background
                    , "1"
                    ]
                    where
                      desc = fmtDesc "Help" myKeymapH 25 lightBlue lightGreen


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings XConfig {XMonad.modMask = modMask} =
  M.fromList 
  [ ((modMask, button1), \w -> focus w >> windows W.swapMaster) -- mod-button1, Raise the window to the top of the stack
  , ((modMask, button2), \w -> focus w >> mouseMoveWindow w) -- mod-button2, Set the window to floating mode and move by dragging
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w) -- mod-button3, Set the window to floating mode and resize by dragging
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
myKeys config = mkKeymap config $ myKeymap ++ rmDesc myKeymapH

myKeymap :: [(String, X ())]
myKeymap = myFloatKeys ++ myWorkspaceMovementKeys 

-- Keys with hints
myKeymapH :: [(String, X (), String)]
myKeymapH =
  concat 
    [
     myControlKeys
    , myLauncherKeys
    , myLayoutKeys
    , myWorkspaceKeys
    , myMovementKeys
    , myMediaKeys
    ]

myWorkspaceMovementKeys =
  [ (prefix ++ key, func ws)
  | (prefix, func) <-
      [ ("M-", windows . W.greedyView) -- go to workspace
      , ("M-S-", windows . viewShift) -- go to workspace taking current window
      , ("M-C-", windows . W.shift) -- send window to workspace
      , ("M-C-S-", swapWithCurrent) -- change workspace number
      ]
  , (key, ws) <- zip keys myWorkspaces
  ]
  where
    keys = fmap return $ ['1' .. '9'] ++ ['-', '=']
    viewShift = liftM2 (.) W.greedyView W.shift

myMovementKeys =
  myWindowMovementKeys ++
  myWorkspaceMovementKeys' ++ myScreenMovementKeys ++ myGotoLayoutKeys

myWindowMovementKeys =
  [ ("M-<D>", windowGo Nav.D, "Focus down")
  , ("M-<U>", windowGo Nav.U, "Focus up")
  , ("M-<L>", windowGo Nav.L, "Focus left")
  , ("M-<R>", windowGo Nav.R, "Focus right")
  , ("M-j", windowGo Nav.D, "Focus down")
  , ("M-k", windowGo Nav.U, "Focus up")
  , ("M-h", windowGo Nav.L, "Focus left")
  , ("M-l", windowGo Nav.R, "Focus right")
  , ("M-S-m", windows W.focusMaster, "Focus master") -- Focus master
  , ("M1-<Tab>", cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab, "Cycle recent windows")
  ]
  where
    windowGo = sendMessage . Nav.Go

myWorkspaceMovementKeys' =
  [ ("M-C-<R>", CycleWS.nextWS, "Next workspace") -- Go to the next
  , ("M-C-<L>", CycleWS.prevWS, "Previos workspace") --  Go to previous workspace
  , ("M-C-l", CycleWS.nextWS, "Next workspace") -- Go to the next
  , ("M-C-h", CycleWS.prevWS, "Previous workspace") --  Go to previous workspace
  ]

myScreenMovementKeys =
  [ ("M-s", CycleWS.nextScreen, "Next screen") -- Move the focus to next screen (multi screen)
  , ("M-o", CycleWS.swapNextScreen, "Swap next screen" )
  , ("M-S-o", CycleWS.shiftNextScreen, "Shift next screen")
  ]

myGotoLayoutKeys =
  [ ("M-g 1", jumpToLayout "Tall", "Layout Tall")
  , ("M-g 2", jumpToLayout "HintedGrid", "Layout HintedGrid")
  , ("M-g 3", jumpToLayout "OneBig", "Layout OneBig")
  , ("M-g 4", jumpToLayout "Circle", "Layout Circle")
  , ("M-g 5", jumpToLayout "Mosaic", "Layout Mosaic")
  , ("M-g 6", jumpToLayout "ThreeCol", "Layout Threecol")
  , ("M-g 7", jumpToLayout "Spiral", "Layout spiral")
  ]
  where
    jumpToLayout = sendMessage . JumpToLayout

myLayoutKeys = myLayoutKeys' ++ myLayoutSwapKeys ++ myLayoutTransformKeys

myLayoutSwapKeys =
  [ ("M-S-<D>", layoutSwap Nav.D, "Swap client down")
  , ("M-S-<U>", layoutSwap Nav.U, "Swap client up")
  , ("M-S-<L>", layoutSwap Nav.L, "Swap client left")
  , ("M-S-<R>", layoutSwap Nav.R, "Swap client right")
  , ("M-S-j", layoutSwap Nav.D, "Swap client down")
  , ("M-S-k", layoutSwap Nav.U, "Swap client up")
  , ("M-S-h", layoutSwap Nav.L, "Swap client left")
  , ("M-S-l", layoutSwap Nav.R, "Swap client right")
  ]
  where
    layoutSwap = sendMessage . Nav.Swap

myLayoutKeys' =
  [ ("M-f", sendMessage $ Toggle NBFULL, "Toggle full screen") -- Toggle Fullscreen mode
  , ("M-C-,", sendMessage $ IncMasterN (-1), "Decrease master") -- Decrease the number of master pane
  , ("M-C-.", sendMessage $ IncMasterN 1, "Increase master") -- Increase the number of master pane
  , ("M-<Space>", sendMessage NextLayout, "Next layout") -- Rotate through the available layout algorithms
  , ("M-m", windows W.shiftMaster, "Shift with master") -- Shift the focused window to the master window
  ]

myLayoutTransformKeys =
  [ ("M-,", sendMessage Shrink, "Decrease horizontally")
  , ("M-.", sendMessage Expand, "Increase vertically")
  , ("M-S-.", sendMessage RTile.MirrorShrink, "Decrease vertically")
  , ("M-S-,", sendMessage RTile.MirrorExpand, "Increase vertically")
  , ("M-g x", sendMessage $ Toggle REFLECTX, "Reflect horizontally")
  , ("M-g y", sendMessage $ Toggle REFLECTY, "Reflect vertically")
  , ("M-g m", sendMessage $ Toggle MIRROR, "Toggle mirror") -- Toggle Mirror layout
  ]

myWorkspaceKeys =
  [ ("M-C-S-<R>", CycleWS.shiftToNext, "Shift to next workspace") -- Shift the focused window to the next workspace
  , ("M-C-S-<L>", CycleWS.shiftToPrev, "Shift to previous workspace") -- Shift the focused window to the previous workspace
  , ("M-C-S-l", CycleWS.shiftToNext, "Shift to next workspace") -- Shift the focused window to the next workspace
  , ("M-C-S-h", CycleWS.shiftToPrev, "Shift to previous workspace") -- Shift the focused window to the previous workspace
  , ("M-<Tab>", CycleWS.toggleWS, "Toggle last workspace") -- toggle last workspace
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
  [ ("M-<Return>", spawn myTerminal, "Terminal") -- Launch terminal
  , ("M-S-<Return>", spawn myFileManager, "File Manager") -- Launch FileManager
  , ("M-' b", spawn myBrowser, "Browser") -- Launch browser
  , ("M-' e", spawn myTextEditor, "Text Editor") -- Launch text editor
  , ("M-' f", spawn myFileManager, "File Manager") -- Launch File Manager 
  , ("M-' k", spawn "xkill", "Kill Window") -- Kill window
  , ("M-' r", spawn myConsoleFileManager, "Ranger") -- Launch text editor
  , ("M-' t", spawn myTmuxTerminal, "Tmux") -- Launch tmux terminal
  , ("M-' v", spawn "nvim", "Neovim") -- Launch text editor
  , ("M-S-C-=", spawn "$HOME/.scripts/xbacklight-toggle.sh", "Toggle backlight")
  ]

myScreenCaptureKeys =
  [ ( "<Print>"
    ,spawn $ myScreenCapture ++ " root && notify-send 'Desktop captured'", "Take a screenshot (desktop)")
  , ( "S-<Print>" 
    ,spawn $ "notify-send 'Select Area';sleep 0.2;" ++ myScreenCapture ++ " area && notify-send 'Area captured'", "Take a screenshot (area)")
  , ( "C-<Print>" -- 
    ,spawn $ myScreenCapture ++ " window && notify-send 'Focused window captured'", "Take a screenshot (window)")
  ]

myMediaKeys =
    -- Play / Pause media
  [ ("<XF86AudioPlay>", spawn "playerctl play-pause", "Media play/pause")
  , ("<XF86AudioStop>", spawn "playerctl pause", "Media pause")
  , ("<XF86AudioPrev>", spawn "playerctl previous", "Media previous")
  , ("<XF86AudioNext>", spawn "playerctl next", "Media next")
  -- Volume
  , ("<XF86AudioRaiseVolume>", spawn "$HOME/.scripts/VolControl.sh up", "Volume up")
  , ("<XF86AudioLowerVolume>", spawn "$HOME/.scripts/VolControl.sh down", "Volume down")
  , ("<XF86AudioMute>", spawn "$HOME/.scripts/XMMute.sh", "Mute")
  -- Brightness
  , ( "<XF86MonBrightnessUp>"
    , spawn "xbacklight + 5 -time 100 -steps 1 && notify-send \"brightness up $(xbacklight -get)\"", "Brightness up")
  , ( "<XF86MonBrightnessDown>"
    , spawn "xbacklight - 5 -time 100 -steps 1 && notify-send \"brightness down $(xbacklight -get)\"", "Brightness down")
  -- Touchpad
  , ("<XF86TouchpadToggle>", spawn "$HOME/.scripts/touchpad_toggle.sh", "Toggle touchpad") -- Touch pad
  -- Browser
  , ("<XF86Explorer>", spawn myBrowser, "Browser") -- Browser
  ]

myControlKeys =
  [ ("M-S-q", kill, "Kill focused") -- Close the focused window
       -- Toggle struts
  , ("M-b", sendMessage ManageDocks.ToggleStruts, "Toggle statusbar")
       -- grid selection
  , ("M-g s", GS.goToSelected myGridSelectConfig, "Grid selection")
       -- Search a window and focus into the window
  , ( "M-g g"
    , WPrompt.windowPrompt myPromptInfix WPrompt.Goto WPrompt.allWindows, "Search and go to client")
       -- Search a window and bring to the current workspace
  , ( "M-g b"
    , WPrompt.windowPrompt myPromptInfix WPrompt.Bring WPrompt.allWindows, "Search and bring client")
  , ("M-g l", myLayoutPrompt, "Layout menu")
       -- Resize viewed windows to the correct size
  --, ("M-n", refresh)
    --  Reset the layouts on the current workspace to default
    --, ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
  , ( "M-C-r" -- Restart xmonad
    , spawn
        "xmonad --recompile && xmonad --restart && notify-send 'Xmonad restarted' || notify-send 'Xmonad failed to restart'", "Compile and restart")
  , ("M-S-r", spawn "xmonad --restart", "Restart") -- restart xmonad w/o recompiling
  , ("M-d", shellPrompt myPrompt, "Shell launcher") -- launch apps
  , ("M1-d", spawn myLauncher, "Launcher") -- launch apps
  , ("M-?", unGrab >> showHelp, "Help")
  , ("M-<Esc>", mySessionPrompt, "Log menu")
  , ("M-u", UH.focusUrgent, "Focus urgent") -- focus urgent window
  , ("M-S-u", UH.clearUrgents, "Clear urgent") -- clear urgents 
  , ("M-<F1>", unGrab >> showHelp, "Show help")
  ]
