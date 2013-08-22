------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
{-# LANGUAGE NoMonomorphismRestriction, ParallelListComp #-}
------------------------------------------------------------------------

import XMonad 
--------------------------------------------------------------------------------
-- Actions imports
--------------------------------------------------------------------------------
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane

--------------------------------------------------------------------------------
-- Hooks imports
--------------------------------------------------------------------------------
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Minimize
import XMonad.Hooks.ToggleHook
--------------------------------------------------------------------------------
-- Layout imports
--------------------------------------------------------------------------------
import XMonad.Layout.Renamed
import XMonad.Layout.Minimize
import XMonad.Layout.Grid
-- import XMonad.Layout.Spacing 
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.IM
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ButtonDecoration
-- import XMonad.Layout.ComboP
-- import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen
-- import XMonad.Layout.LayoutBuilder -- construye contenedores
import XMonad.Layout.Magnifier -- incrementa la ventana enfocada
import XMonad.Layout.Tabbed
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
import XMonad.Layout.ShowWName
import XMonad.Layout.FixedColumn
import XMonad.Layout.WorkspaceDir

--------------------------------------------------------------------------------
-- Others from Xmonad
--------------------------------------------------------------------------------
-- import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Layout

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- Others
--------------------------------------------------------------------------------
import Control.Monad (liftM2)
import Data.List
-- import DBus.Client
-- import System.Taffybar.XMonadLog ( dbusLog )

--------------------------------------------------------------------------------
-- Main Xmonad
--------------------------------------------------------------------------------
main :: IO ()
main = do
  taffy <- spawnPipe "~/.cabal/bin/taffybar"
--    _ <- spawnPipe "tint2 -c /home/eleazar/.config/tint2/tint2rc"
  _ <- spawnPipe "bash /home/eleazar/conky-manager/conky-startup.sh"
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/xmobar.hs"

  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 0
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , logHook            = ppXbar xmproc 
                           <+> myLogHook
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
myTerminal = "gnome-terminal"
myEditor = "terminator -l yi"
myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces =
  [
    "Any", "Not", "Nev",
    "Term", "Expl", "Game",
    "Docu", "Hask", "Web",
    "Chat", "Play", "Inks"
  ]

-- | On start xmonad, set this workspace how current
startupWorkspace :: [Char]
startupWorkspace = "Hask"

-- | fullscreen support, don't hide dock (xmobar, taffybar), minimize window support
myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook 

-- | set opacity to unfocused windows, add app property for set up the opacity 
myFadeHook :: FadeHook
myFadeHook = composeAll [ opaque -- **Important that this is in first place
                        , isUnfocused --> opacity 0.75 -- all unfocused
                        , className =? "mplayer2"  --> opaque -- example ^
                        ]

-- | 
myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D"
        startupHook defaultConfig
        spawnOnce myAppOnStartup
        windows $ W.greedyView startupWorkspace

-- | App on start, more easy form to a start-hook.sh
myAppOnStartup :: [Char]
myAppOnStartup = flip (++) "&" . intercalate " &\n" $
      [ "xcompmgr",
        "clementine",
        "nm-applet",
        "guake",
        "synapse",
        "nautilus -n",
        "gnome-sound-applet",
        "/usr/bin/gnome-keyring-daemon --start --components=ssh,secrets,gpg,pkcs11",
        "/usr/libexec/gnome-settings-daemon",
        "/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1",
        "xdg-user-dirs-gtk-update",
        "/opt/extras.ubuntu.com/my-weather-indicator/bin/my-weather-indicator"
        -- "taffybar -c=configfile -- TODO
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
                , ppHidden = xmobarColor "green" ""
                , ppTitle = xmobarColor "white" "" . shorten 39
                , ppCurrent = xmobarColor "yellow" ""
                , ppOrder = (\(ws:l:t:xs) -> [t,l,ws])
                , ppSep = "   "}

--------------------------------------------------------------------------------
-- Layouts
-- 
myLayouts = onWorkspace "Chat" pidginLayout $ 
            onWorkspace "Hask" codeLayouts $ defaultLayouts


tiledLayout = Tall nmaster delta ratio
  where
    nmaster = 1      -- The default number of windows in the master pane.
    ratio   = 1/2    -- Default proportion of screen occupied by master pane.
    delta   = 3/100  -- Percent of screen to increment by when resizing panes.
fixedLayout = FixedColumn 1 20 80 10
codeLayouts = avoidStruts tiledLayout ||| (avoidStruts (Mirror tiledLayout))
pidginLayout = avoidStruts $ withIM (18/100) (Role "buddy_list") Grid

scratchpads = [ NS "fvim" "gvim --role fvim" (role =? "fvim") defaultFloating,
                NS "terminal" "gnome-terminal --role terminal" (role =? "terminal") defaultFloating
              ] where role = stringProperty "WM_WINDOW_ROLE"

defaultLayouts = 
            named "Grid"      Grid
	||| named "Full"      Full 
        ||| named "Mirror"    (Mirror $ ResizableTall 1 (3/100) (1/2) [])
        ||| named "Magnifier" (magnifier $ ResizableTall 1 (3/100) (1/2) [])
        ||| named "Buttons"   (buttonDeco shrinkText defaultThemeWithButtons (layoutHook defaultConfig))
        ||| named "Tabbed"    (tabbed shrinkText defaultTheme)
        where named x = renamed [Replace x] . minimize . avoidStruts

-- Window rules:
myManageHook :: ManageHook
myManageHook = composeAll . concat $
             [ [isDialog --> doCenterFloat
               ,isFullscreen --> doFullFloat]]
             ++ [inWorksp (const doIgnore) () myIgnores]
             ++ [inWorksp doShiftAndGo w s    | w <- myWorkspaces | s <- myShifts ]
             ++ [inWorksp (const doFloat) () myFloats]

         where doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
               inWorksp d w s = [(className =? x <||> title =? x <||> resource =? x) --> (d w) | x <- s]
               myShifts = [[], [], [], my1Shifts, my2Shifts, my3Shifts, my4Shifts, my5Shifts, my6Shifts, my7Shifts, my8Shifts, my9Shifts]
               myFloats = ["Notas adhesivas", "Terminator Preferences", "Guake", "guake.py", "Escritorio","notification-daemon"]
               myIgnores = ["notification-deamon", "Conky", "gnome-panel"]
               my1Shifts = [myTerminal]
               my2Shifts = ["nautilus", "ranger", "dolphin"]
               my3Shifts = []
               my4Shifts = ["evince"]
               my5Shifts = ["gedit", "emacs24","SublimeText", "terminator", "gvim", "leksah", "Yi"]
               my6Shifts = ["chromium-browser"]
               my7Shifts = ["emphaty","quassel", "thunderbird", "Pidgin"]
               my8Shifts = ["clementine", "banshee"]
               my9Shifts = ["inkscape"]

--------------------------------------------------------------------------------
-- Keybinding
--------------------------------------------------------------------------------
-- GSConfig options:
myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_font = "" ++ myFont ++ ""
    }

-- XPConfig options:
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { promptBorderWidth = 1
                             , position = Top
                             , height = 30
                             , historySize = 100
                             }

-- | M mk ref to mymodmask, C -> Ctrl, S -> Shift, M1 -> Alt
myKeyBindings :: [(String, X ())]
myKeyBindings = [ ( "M-b",               sendMessage ToggleStruts) -- Set a current app in full screen, hide all bars
                , ( "M-a",               sendMessage MirrorShrink)
                , ( "M-z",               sendMessage MirrorExpand)
                , ( "M-m",               withFocused minimizeWindow) -- Minimize app
                , ( "M-S-m",             sendMessage RestoreNextMinimizedWin) -- UnMinimize app
                , ( "M-C-<KP_Add>",      sendMessage MagnifyMore) -- Zoom in focus app  -- Magnified layout
                , ( "M-C-<KP_Subtract>", sendMessage MagnifyLess) -- Zoom out focus app --
                , ( "M-C-m",             sendMessage Toggle     ) -- On/Off the ZoomEnd -- Magnified layout
                , ( "M-S-g",             gridselectWorkspace myGSConfig (\ws -> W.greedyView ws . W.shift ws)) -- display grid select and go to selected workspace
                , ( "M-g",               goToSelected myGSConfig) -- display grid select and go to selected window
                , ( "M-q",               spawn "killall dzen2 ; killall conky ; killall tint2 ; killall taffybar-linux-x86_64; xmonad --recompile && xmonad --restart")
                , ( "M-<Print>",         spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
                , ( "M-f",               spawn "synapse") -- launcher
                , ( "M-x",               runOrRaisePrompt myXPConfig) -- alternative to synapse
                , ( "M-l",               layoutPrompt myXPConfig) -- launcher for layout
                , ( "M-S-x",             changeDir myXPConfig) -- en que situaciones sirve???
                , ( "M-t",               withFocused $ windows . W.sink) -- rehook app to layout if this is float
                , ( "M-c",               spawn "chromium-browser")
                , ( "M-n",               spawn "nautilus")
                , ( "M-e",               spawn myEditor)
                , ( "M-u",               focusUrgent) -- me redirige al foco urgente
                , ( "M-S-t",             namedScratchpadAction scratchpads "terminal")
                , ( "M-S-v",             namedScratchpadAction scratchpads "fvim")
                , ( "<XF86AudioMute>",        spawn "amixer -q set Master toggle")
                , ( "<XF86AudioLowerVolume>", spawn "amixer -q set Master 1%-")
                , ( "<XF86AudioRaiseVolume>", spawn "amixer -q set Master 1%+")
                ]   


-- TECLADO NUMERICO
-- | There are same with num bloq or without
numPadKeys :: [String]
numPadKeys = [ "<KP_Insert>", "<KP_Delete>", "<KP_Enter>"
             , "<KP_End>",    "<KP_Down>",   "<KP_Page_Down>"
             , "<KP_Left>",   "<KP_Begin>",  "<KP_Right>"
             , "<KP_Home>",   "<KP_Up>",     "<KP_Page_Up>"
             ]

numKeys :: [String]
numKeys = [ "0", "<KP_Decimal>", "<KP_Equal>"
          , "1", "2", "3"
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
