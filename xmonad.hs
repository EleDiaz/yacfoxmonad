------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}

------------------------------------------------------------------------
import XMonad

--------------------------------------------------------------------------------
-- Actions imports
--------------------------------------------------------------------------------
import XMonad.Actions.Plane

--------------------------------------------------------------------------------
-- Hooks imports
--------------------------------------------------------------------------------
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

--------------------------------------------------------------------------------
-- Layout imports
--------------------------------------------------------------------------------
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.DecorationMadness
import XMonad.Layout.MagicFocus

--------------------------------------------------------------------------------
-- Others from Xmonad
--------------------------------------------------------------------------------
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

--------------------------------------------------------------------------------
-- Others
--------------------------------------------------------------------------------
import Control.Monad (liftM2)
import Data.List
import Data.Monoid
import Data.Ratio

--------------------------------------------------------------------------------
-- Main Xmonad
--------------------------------------------------------------------------------
main :: IO ()
main = do
  _ <- spawnPipe "~/.local/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $
    withUrgencyHook NoUrgencyHook $
    def
    { terminal = myTerminal
    , focusFollowsMouse = False
    , borderWidth = 0
    -- , focusedBorderColor = "#000000"
    , modMask = myModMask
    , workspaces = myWorkspaces
    , logHook = myLogHook >> ppXbar >>= xmonadPropLog
    , layoutHook = myLayouts
    , manageHook =
      myManageHook <+>
      manageDocks <+>
      dynamicMasterHook <+>
      toggleHook "float" doFloat <+> namedScratchpadManageHook scratchpads <+> placeHook (fixed (1 % 2, 1 % 2))
    , handleEventHook = myHandleEventHook
    , startupHook = myStartupHook >> checkKeymap def myKeys
    } `additionalKeysP`
    myKeys

myTerminal :: String
myTerminal = "gnome-terminal" -- best support for colors
-- myEditor = "gnome-terminal -e 'fish -c nvim'"
-- myEditor2 = "vim"
myModMask :: KeyMask
myModMask = mod4Mask

term, files, game, docs, hask, web, chat, play, ink :: String
term = "\61728"
files = "\61637"
game = "\61723"
docs = "\61889"
hask = "\59255"
web = "\62056"
chat = "\61670"
play = "\61441"
ink = "\61948"

myWorkspaces :: [String]
myWorkspaces =
  [
    term, files, game, --   
    docs, hask, web,   --   
    chat, play, ink    --   
  ]

-- | On start xmonad, set this workspace as current on start up
startupWorkspace :: String
startupWorkspace = hask

-- | fullscreen support, minimize window support
myHandleEventHook :: Event -> X All
myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook

-- | set opacity to unfocused windows, add app property for set up the opacity
myFadeHook :: FadeHook
myFadeHook =
  composeAll
    [ opaque -- **Important** It should be in first place
    , isUnfocused --> opacity 0.80 -- all unfocused
    , className =? "mplayer2" --> opaque -- example ^
    , className =? "Emacs" --> opaque
    , title =? "Speedbar 1.0" --> opaque]

-- |
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  startupHook def
  spawnOnce myAppOnStartup
  windows $ W.greedyView startupWorkspace

-- | App on start
myAppOnStartup :: String
myAppOnStartup =
  flip (++) "&" . intercalate " &\n" $
  [ "compton"
  , "stalonetray --window-strut none --sticky -t --geometry 1x1+0-0 --grow-gravity W"
  , "nm-applet"
  , "nitrogen --restore"
  , "gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh"
    --, "taffybar"
    --, "lxqt-panel"
    --, "yabar"
  , "qasmixer -t"
  , "~/Telegram/Telegram"
   ]

myLogHook :: X ()
myLogHook =
  fadeWindowsLogHook myFadeHook <+> ewmhDesktopsLogHook >> setWMName "LG3D"

--------------------------------------------------------------------------------
-- Theme
--------------------------------------------------------------------------------
gray = "#777777"
red  = "#a9212e"
black = "black"
white = "white"
green = "green"
-- black = "black"

eleTheme :: Theme
eleTheme =
  def
  { inactiveBorderColor = gray
  , inactiveColor = gray
  , inactiveTextColor = white
  , activeBorderColor = red
  , activeColor = red
  , activeTextColor = white
  , fontName = "xft:FantasqueSansMono Nerd Font-13"
  , decoHeight = 18
  , urgentColor = green
  , urgentTextColor = white
  }

-- | Config of xmobar pp
ppXbar :: X String
ppXbar =
  dynamicLogString . namedScratchpadFilterOutWorkspacePP $
  xmobarPP
  { ppHiddenNoWindows = xmobarColor gray "" -- "#7e1188"
  , ppHidden = id
  , ppTitle = xmobarColor white "" . shorten 50
  , ppCurrent = xmobarColor red ""
  , ppUrgent = xmobarColor green "" . (++ "\6177") -- 
  , ppOrder =
    \(ws:l:t:[]) ->
       [ t
       , "<fc=white,black>\57522</fc><fc=#a9212e,white>"
       , l
       , "</fc><fc=black,white>\57522</fc>"
       , ws -- 
        ]
  , ppSep = " "
  , ppWsSep = xmobarColor red black " \61762 " --  ""
  }

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------
named x = renamed [Replace x] . minimize . avoidStruts


myLayouts =
  onWorkspace chat circleLayout $
  onWorkspace hask codeLayouts $
  onWorkspace play circleLayout $ onWorkspace web fullLayout defaultLayouts

columns = "\61659" -- 
tiledLayout = named columns $ Tall 1 (1 / 2) (50 / 100)

tabs = "\61563" -- 
tabbedLayout = named tabs (tabbed shrinkText eleTheme)

amplified = "\61451" -- 
magnifierLayout =
  named amplified (magnifier $ ResizableTall 1 (3 / 100) (1 / 2) [])

full = "\61618" -- 
fullLayout = named full Full ||| magnifierLayout

circle = "\61473" -- 
circleLayout = named circle $ magicFocus $ circleDefault shrinkText eleTheme

code = "\61729" -- 
codeLayouts =
  fullLayout ||| defaultLayouts-- named "Emacs" (withIM (15 / 100) (Title "Speedbar 1.0") Grid)

defaultLayouts = tiledLayout ||| magnifierLayout ||| tabbedLayout ||| circleLayout

-- pidginLayout = named "Chat" (withIM (35/100) (Not (Role "ConversationsWindow")) Grid)

role' = stringProperty "WM_WINDOW_ROLE"

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "terminal" "gnome-terminal --role=scratch" (role' =? "scratch") (customFloating $ W.RationalRect 0 0 1 (15 / 50))
  , NS "Kashe" "/home/elediaz/.xmonad/Kashe" (className =? "Kashe") box]
  where
    box = customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

-- Window rules:
myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
  [[isDialog --> doCenterFloat, isFullscreen --> doFullFloat]] ++
  [inWorksp (const doIgnore) () myIgnores] ++
  [inWorksp doShiftAndGo w s | w <- myWorkspaces | s <- myShifts ]
   ++
  [inWorksp (const doFloat) () myFloats]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    inWorksp d w s =
      [ (className =? x <||> title =? x <||> role' =? x) --> d w
      | x <- s ]
    myShifts = [my1Shifts, my2Shifts, my3Shifts, my4Shifts, my5Shifts, my6Shifts, my7Shifts, my8Shifts, my9Shifts]
    myFloats = ["Terminator Preferences", "notification-daemon", "plasma-desktop", "klipper", "lxqt-panel"]
    myIgnores = ["xfce4-notifyd", "conky", "gnome-panel", "oblogout"]
    my1Shifts = []
    my2Shifts = ["ark", "nautilus", "thunar", "ranger", "dolphin"]
    my3Shifts = []
    my4Shifts = ["evince", "okular", "zathura"]
    my5Shifts = ["gedit", "emacs", "leksah", "Yi"]
    my6Shifts = ["browser"]
    my7Shifts = ["emphaty", "quassel", "thunderbird", "Pidgin", "skype", "Telegram"]
    my8Shifts = ["clementine", "banshee", "rhythmbox", "Google Play Music", "qasmixer"]
    my9Shifts = ["inkscape", "blender"]
--------------------------------------------------------------------------------
-- Keybinding
--------------------------------------------------------------------------------

-- | M mk ref to mymodmask, C -> Ctrl, S -> Shift, M1 -> Alt
myKeyBindings :: [(String, X ())]
myKeyBindings =
  [ ("M-b", sendMessage ToggleStruts) -- Set a current app in fullscreen, hide all bars
  , ("M-m", withFocused minimizeWindow) -- Minimize app
  , ("M-S-m", sendMessage RestoreNextMinimizedWin) -- UnMinimize app
  , ("M-C-<KP_Add>", sendMessage MagnifyMore) -- Zoom in focus app  -- Magnified layout
  , ("M-C-<KP_Subtract>", sendMessage MagnifyLess) -- Zoom out focus app --
  , ("M-C-m", sendMessage Toggle) -- On/Off the ZoomEnd -- Magnified layout
  , ("M-g", spawn "rofi -show window") -- Switch between windows
  , ( "M-q"
    , spawn $
      concat
        [ "killall dzen2 ;"
        , "killall conky ;"
        , "killall xmobar;"
        , "xmonad --recompile && xmonad --restart"])
  , ("M-f", spawn "rofi -combi-modi drun,run -show combi") -- launcher
  , ("M-t", withFocused $ windows . W.sink) -- rehook app to layout if this is float
  , ("M-u", focusUrgent) -- me redirige al foco urgente
  , ("M-S-t", namedScratchpadAction scratchpads "terminal")
  , ("M-S-v", namedScratchpadAction scratchpads "fvim")
  , ("M-S-a", namedScratchpadAction scratchpads "Kashe")
    -- , ( "<XF86AudioMute>",        spawn "amixer -c 1 set Master toggle") -- Always a disaster make works
    -- , ( "<XF86AudioLowerVolume>", spawn "amixer -c 1 set Master 1%-")
    -- , ( "<XF86AudioRaiseVolume>", spawn "amixer -c 1 set Master 1%+")
    --, ( "<XF86AudioPlay>",        spawn "mpc play") -- TODO: play and pause
    --, ( "<XF86AudioStop>",        spawn "mpc stop")
    --, ( "<XF86AudioPrev>",        spawn "mpc prev")
    --, ( "<XF86AudioNext>",        spawn "mpc next")
   ]

-- TECLADO NUMERICO
-- | There are same with num bloq or without
numPadKeys :: [String]
numPadKeys =
  [ "<KP_End>",    "<KP_Down>",   "<KP_Page_Down>"
  , "<KP_Left>",   "<KP_Begin>",  "<KP_Right>"
  , "<KP_Home>",   "<KP_Up>",     "<KP_Page_Up>"
  ]

numKeys :: [String]
numKeys =
  [ "1", "2", "3"
  , "4", "5", "6"
  , "7", "8", "9"
  ]

-- | Plane Keys, move between workspaces with arrow keys, and with shift+Arrow, transport a window
myplaneKeys :: Lines -> Limits -> [(String, X ())]
myplaneKeys ln limits =
  [ (keyMask ++ keySym, function ln limits direction)
  | (keySym, direction) <-
     zip ["<Left>", "<Down>", "<Right>", "<Up>"] $ enumFrom ToLeft
  , (keyMask, function) <- [("M-", planeMove), ("M-S-", planeShift)] ]

myKeys :: [(String, X ())]
myKeys =
  [ (m ++ "M-" ++ k, windows $ f i)
  | (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
  , (i, k) <- zip myWorkspaces numPadKeys ++ zip myWorkspaces numKeys ] ++
  myKeyBindings ++ myplaneKeys (Lines 4) Circular
