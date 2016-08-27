------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE FlexibleContexts          #-}
------------------------------------------------------------------------

import           XMonad
--------------------------------------------------------------------------------
-- Actions imports
--------------------------------------------------------------------------------
import           XMonad.Actions.Plane

--------------------------------------------------------------------------------
-- Hooks imports
--------------------------------------------------------------------------------
import           XMonad.Hooks.DynamicHooks
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops   hiding (fullscreenEventHook)
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.ToggleHook
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.Place

--------------------------------------------------------------------------------
-- Layout imports
--------------------------------------------------------------------------------
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Minimize
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.MagicFocus

--------------------------------------------------------------------------------
-- Others from Xmonad
--------------------------------------------------------------------------------

import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

--------------------------------------------------------------------------------
-- Others
--------------------------------------------------------------------------------
import           Control.Monad               (liftM2)
import           Data.List
import           Data.Monoid
import           Data.Ratio

--------------------------------------------------------------------------------
-- Main Xmonad
--------------------------------------------------------------------------------
main :: IO ()
main = do
  _ <- spawnPipe "~/.local/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $ withUrgencyHook NoUrgencyHook $ def
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 0
    -- , focusedBorderColor = "#000000"
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , logHook            = myLogHook >> ppXbar >>= xmonadPropLog
    , layoutHook         = myLayouts
    , manageHook         = myManageHook
                           <+> manageDocks
                           <+> dynamicMasterHook
                           <+> toggleHook "float" doFloat
                           <+> namedScratchpadManageHook scratchpads
                           <+> placeHook (fixed (1%2, 1%2))
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook >> checkKeymap def myKeys
    }
     `additionalKeysP` myKeys

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
myFadeHook = composeAll [ opaque -- **Important that this is in first place
                        , isUnfocused --> opacity 0.80 -- all unfocused
                        , className =? "mplayer2"  --> opaque -- example ^
                        , className =? "Emacs"  --> opaque
                        , title =? "Speedbar 1.0"  --> opaque
                        ]

-- |
myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D"
        startupHook def
        spawnOnce myAppOnStartup
        windows $ W.greedyView startupWorkspace

-- | App on start, more easy form to a start-hook.sh
myAppOnStartup :: String
myAppOnStartup = flip (++) "&" . intercalate " &\n" $
      [ "compton"
      , "stalonetray -v --window-strut none --sticky --tint-color black"
      , "nm-applet"
      , "nitrogen --restore"
      , "gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh"
      --, "taffybar"
      --, "lxqt-panel"
      --, "yabar"
      , "qasmixer -t"
      , "~/Telegram/Telegram"
      -- "pa-applet",
      -- "octopi-notifier", -- optimizando energia :\
      -- "parcellite"
      ]

myLogHook :: X ()
myLogHook = fadeWindowsLogHook myFadeHook
            <+> ewmhDesktopsLogHook
            >> setWMName "LG3D"

-- | Config of xmobar pp
ppXbar :: X String
ppXbar = dynamicLogString . namedScratchpadFilterOutWorkspacePP $ xmobarPP
    { -- ppOutput = hPutStrLn bar
      ppHiddenNoWindows = xmobarColor "#777777" ""-- "#7e1188"
    , ppHidden = id
    , ppTitle = xmobarColor "white" "" . shorten 50
    , ppCurrent = xmobarColor "#a9212e" ""
    , ppUrgent = xmobarColor "green" "" . (++"\6177") -- 
    , ppOrder = \(ws:l:t:_) -> [t,"<fc=white,black>\57522</fc><fc=#a9212e,white>",l,"</fc><fc=black,white>\57522</fc>",ws] --   
    , ppSep = " "
    , ppWsSep = "<fc=#a9212e,black> \61762 </fc>"--  ""
    }

--------------------------------------------------------------------------------
-- Layouts
--
named x = renamed [Replace x] . minimize . avoidStruts


myLayouts = onWorkspace chat circleLayout $
            onWorkspace hask codeLayouts $
            onWorkspace files explLayout $
            onWorkspace web  (named "Web" Full) defaultLayouts

tiledLayout = named "Tiled" $ Tall nmaster delta ratio
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.
tabbedLayout = named "Tabbed" (tabbed shrinkText defaultTheme)
magnifierLayout = named "Magnifier" (magnifier $ ResizableTall 1 (3/100) (1/2) [])


explLayout = tabbedLayout ||| magnifierLayout

codeLayouts = named "Code" Full ||| named "Emacs" (avoidStruts (withIM (15/100) (Title "Speedbar 1.0") Grid))

pidginLayout = named "Chat" $ avoidStruts $ withIM (35/100) (Not (Role "ConversationsWindow")) Grid

circle = "\61473" -- 
circleLayout = named circle $ magicFocus circleSimpleDefault

defaultLayouts = tiledLayout
              ||| magnifierLayout
              ||| tabbedLayout

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "terminal" "gnome-terminal --role=scratch" (role' =? "scratch") (customFloating $ W.RationalRect 0 0 1 (15/50))
              , NS "Kashe" "/home/elediaz/.xmonad/Kashe" (className =? "Kashe") box
              ] where role' = stringProperty "WM_WINDOW_ROLE"
                      box  = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

-- Window rules:
myManageHook :: ManageHook
myManageHook = composeAll . concat $
             [ [isDialog --> doCenterFloat
               ,isFullscreen --> doFullFloat]]
             ++ [inWorksp (const doIgnore) () myIgnores]
             ++ [inWorksp doShiftAndGo w s    | w <- myWorkspaces | s <- myShifts ]
             ++ [inWorksp (const doFloat) () myFloats]

         where doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
               inWorksp d w s = [(className =? x <||> title =? x <||> resource =? x) --> d w | x <- s]
               myShifts = [my1Shifts, my2Shifts, my3Shifts, my4Shifts, my5Shifts, my6Shifts, my7Shifts, my8Shifts, my9Shifts]
               myFloats = ["Notas adhesivas", "Terminator Preferences", "Guake", "guake.py", "Escritorio","notification-daemon"
                          , "plasma-desktop", "klipper", "lxqt-panel"]
               myIgnores = ["xfce4-notifyd", "conky", "gnome-panel", "oblogout"]
               my1Shifts = []
               my2Shifts = ["ark", "nautilus", "thunar", "ranger", "dolphin"]
               my3Shifts = []
               my4Shifts = ["evince", "okular", "zathura"]
               my5Shifts = ["gedit", "emacs","sublime_text", "gvim", "leksah", "Yi"]
               my6Shifts = ["chromium-browser"]
               my7Shifts = ["emphaty","quassel", "thunderbird", "Pidgin", "skype", "Telegram"]
               my8Shifts = ["clementine", "banshee", "rhythmbox"]
               my9Shifts = ["inkscape", "blender"]

--------------------------------------------------------------------------------
-- Keybinding
--------------------------------------------------------------------------------
{- Old bindings not used
                , ( "M-a",               sendMessage MirrorShrink)
                , ( "M-z",               sendMessage MirrorExpand)
                , ( "M-S-g",             gridselectWorkspace myGSConfig (\ws -> W.greedyView ws . W.shift ws)) -- display grid select and go to selected workspace
                , ( "M-g",               goToSelected myGSConfig) -- display grid select and go to selected window
                , ( "M-x",               runOrRaisePrompt myXPConfig) -- alternative to synapse
                , ( "M-l",               layoutPrompt myXPConfig) -- launcher for layout
                , ( "M-S-x",             changeDir myXPConfig) -- en que situaciones sirve???
                , ( "M-w",               spawn "oblogout")
                , ( "M-n",               spawn "dolphin")
                , ( "M-p",               spawn "xprop | ~/xmonadpropwrite")
                , ( "C-M-t",             spawn "gksu \"modprobe -i psmouse\"") -- activa touchpad -- To TestMODE
                , ( "C-M-S-t",           spawn "gksu \"modprobe -r psmouse\"") -- disable touchpad
                , ( "M-c",               spawn "chromium-browser")
                , ( "M-e",               spawn myEditor)
                , ( "M-<Print>",         spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
-}

-- | M mk ref to mymodmask, C -> Ctrl, S -> Shift, M1 -> Alt
myKeyBindings :: [(String, X ())]
myKeyBindings = [ ( "M-b",               sendMessage ToggleStruts) -- Set a current app in fullscreen, hide all bars
                , ( "M-m",               withFocused minimizeWindow) -- Minimize app
                , ( "M-S-m",             sendMessage RestoreNextMinimizedWin) -- UnMinimize app
                , ( "M-C-<KP_Add>",      sendMessage MagnifyMore) -- Zoom in focus app  -- Magnified layout
                , ( "M-C-<KP_Subtract>", sendMessage MagnifyLess) -- Zoom out focus app --
                , ( "M-C-m",             sendMessage Toggle     ) -- On/Off the ZoomEnd -- Magnified layout
                , ( "M-g",               spawn "rofi -show window") -- Switch between windows
                , ( "M-q",               spawn $ concat [ "killall dzen2 ;"
                                                        , "killall conky ;"
                                                        , "killall xmobar;"
                                                        , "xmonad --recompile && xmonad --restart"])
                , ( "M-f",               spawn "rofi -show drun") -- launcher
                , ( "M-t",               withFocused $ windows . W.sink) -- rehook app to layout if this is float
                , ( "M-u",               focusUrgent) -- me redirige al foco urgente
                , ( "M-S-t",             namedScratchpadAction scratchpads "terminal")
                , ( "M-S-v",             namedScratchpadAction scratchpads "fvim")
                , ( "M-S-a",             namedScratchpadAction scratchpads "Kashe")
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
numPadKeys = [ "<KP_End>",    "<KP_Down>",   "<KP_Page_Down>"
             , "<KP_Left>",   "<KP_Begin>",  "<KP_Right>"
             , "<KP_Home>",   "<KP_Up>",     "<KP_Page_Up>"
             ]

numKeys :: [String]
numKeys = [ "1", "2", "3"
          , "4", "5", "6"
          , "7", "8", "9"
          ]

-- | Plane Keys, move between workspaces with arrow keys, and with shift+Arrow, transport a window
myplaneKeys :: Lines -> Limits -> [(String, X ())]
myplaneKeys ln limits =
  [ (keyMask++keySym, function ln limits direction)
  | (keySym, direction) <- zip ["<Left>", "<Down>", "<Right>", "<Up>"] $ enumFrom ToLeft
  , (keyMask, function) <- [("M-", planeMove), ("M-S-", planeShift)]
  ]


myKeys :: [(String, X ())]
myKeys = [ (m++"M-"++k, windows $ f i) | (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
                                       , (i, k) <- zip myWorkspaces numPadKeys ++ zip myWorkspaces numKeys
         ] ++ myKeyBindings ++ myplaneKeys (Lines 4) Circular
