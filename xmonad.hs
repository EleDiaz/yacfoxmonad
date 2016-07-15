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
import           XMonad.Actions.GridSelect
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
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WorkspaceDir

--------------------------------------------------------------------------------
-- Others from Xmonad
--------------------------------------------------------------------------------
-- import XMonad.Prompt.Shell
import           XMonad.Prompt
import           XMonad.Prompt.Layout
import           XMonad.Prompt.RunOrRaise

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
-- import DBus.Client
-- import System.Taffybar.XMonadLog ( dbusLog )

--------------------------------------------------------------------------------
-- Main Xmonad
--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- conky <- spawnPipe "conky"
  -- xmproc <- spawnPipe "/home/elediaz/.cabal/bin/xmobar /home/elediaz/.xmonad/xmobar.hs"
  -- xmbar  <- spawnPipe "/home/elediaz/.cabal/bin/xmobar /home/elediaz/.xmonad/bar2.hs"
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 0
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , logHook            = myLogHook
    , layoutHook         = showWName myLayouts
    , manageHook         = myManageHook
                           <+> manageDocks
                           <+> dynamicMasterHook
                           <+> toggleHook "float" doFloat
                           <+> namedScratchpadManageHook scratchpads
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook >> checkKeymap defaultConfig myKeys
    }
     `additionalKeysP` myKeys

myTerminal, myEditor :: String
myTerminal = "gnome-terminal" -- best support for colors
myEditor = "gnome-terminal -e 'fish -c nvim'"
myEditor2 = "vim"
myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces =
  [
    "Term", "Expl", "Game",
    "Docu", "Hask", "Web",
    "Chat", "Play", "Inks"
  ]

-- | On start xmonad, set this workspace as current on start up
startupWorkspace :: [Char]
startupWorkspace = "Hask"

-- | fullscreen support, minimize window support
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
        startupHook defaultConfig
        spawnOnce myAppOnStartup
        windows $ W.greedyView startupWorkspace

-- | App on start, more easy form to a start-hook.sh
myAppOnStartup :: String
myAppOnStartup = flip (++) "&" . intercalate " &\n" $
      [ "compton"
      , "nm-applet"
      , "nitrogen --restore"
      , "gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh"
      , "taffybar"
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
--ppXbar :: Handle -> X ()
ppXbar bar = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
                { ppOutput = hPutStrLn bar
                , ppHiddenNoWindows = id
                , ppHidden = xmobarColor "yellow" ""
                , ppTitle = xmobarColor "white" "" . shorten 39
                , ppCurrent = xmobarColor "red" ""
                , ppOrder = (\(ws:l:t:xs) -> [t,l,ws])
                , ppSep = "   "}

--------------------------------------------------------------------------------
-- Layouts
--
named x = renamed [Replace x] . minimize . avoidStruts


myLayouts = onWorkspace "Chat" pidginLayout $
            onWorkspace "Hask" codeLayouts $
            onWorkspace "Expl" explLayout $
            onWorkspace "Web"  (named "Web" Full) defaultLayouts

tiledLayout = named "Tiled" $ Tall nmaster delta ratio
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.
tabbedLayout = named "Tabbed" (tabbed shrinkText defaultTheme)
magnifierLayout = named "Magnifier" (magnifier $ ResizableTall 1 (3/100) (1/2) [])


explLayout = tabbedLayout ||| magnifierLayout

codeLayouts = named "Code" Full ||| (named "Emacs" $ avoidStruts (withIM (15/100) (Title "Speedbar 1.0") Grid))

pidginLayout = named "Chat" $ avoidStruts $ withIM (35/100) (Not (Role "ConversationsWindow")) Grid

defaultLayouts = tiledLayout
              ||| magnifierLayout
              ||| tabbedLayout


scratchpads = [ NS "fvim" "gvim --role fvim" (role =? "fvim") box
              , NS "terminal" "terminator -T scratch"
                       (title =? "scratch")
                       (customFloating $ W.RationalRect 0 (3/5) 1 (20/50))
              , NS "Kashe" "/home/elediaz/.xmonad/Kashe" (className =? "Kashe") box
              , NS "Pomodoro" "gnome-clocks"
                       (title =? "Relojes")
                       (customFloating $ W.RationalRect (1/20) (1/20) (18/20) (18/20))
              ] where role = stringProperty "WM_WINDOW_ROLE"
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
                          , "plasma-desktop", "klipper"]
               myIgnores = ["xfce4-notifyd", "conky", "gnome-panel", "oblogout"]
               my1Shifts = ["Terminal"]
               my2Shifts = ["ark", "nautilus", "thunar", "ranger", "dolphin"]
               my3Shifts = []
               my4Shifts = ["evince", "okular", "zathura"]
               my5Shifts = ["gedit", "emacs","sublime_text", "gvim", "leksah", "Yi"]
               my6Shifts = ["chromium-browser"]
               my7Shifts = ["emphaty","quassel", "thunderbird", "Pidgin", "skype"]
               my8Shifts = ["clementine", "banshee", "rhythmbox"]
               my9Shifts = ["inkscape", "blender"]

--------------------------------------------------------------------------------
-- Keybinding
--------------------------------------------------------------------------------

-- GSConfig options:
myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig
             { gs_cellheight = 50
             , gs_cellwidth = 150
             , gs_cellpadding = 10
             , gs_font = "xft:Ubuntu-11"
             }

-- XPConfig options:
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
             { promptBorderWidth = 1
             , position = Top
             , height = 30
             , historySize = 10
             , font = "xft:Ubuntu-11"
             , autoComplete = Just 1
             }

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
                                                        , "; killall xmobar;"
                                                        , "xmonad --recompile && xmonad --restart"])
                , ( "M-<Print>",         spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
                , ( "M-f",               spawn "rofi -show run") -- launcher
                , ( "M-t",               withFocused $ windows . W.sink) -- rehook app to layout if this is float
                , ( "M-c",               spawn "chromium-browser")
                , ( "M-e",               spawn myEditor)
                , ( "M-u",               focusUrgent) -- me redirige al foco urgente
                , ( "M-S-t",             namedScratchpadAction scratchpads "terminal")
                , ( "M-S-v",             namedScratchpadAction scratchpads "fvim")
                , ( "M-S-a",             namedScratchpadAction scratchpads "Kashe")
                , ( "M-S-p",             namedScratchpadAction scratchpads "Pomodoro")
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
                                       , (i, k) <- zip myWorkspaces numPadKeys ++ (zip myWorkspaces numKeys)
         ] ++ myKeyBindings ++ (myplaneKeys (Lines 4) Circular)
