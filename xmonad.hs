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
import qualified XMonad.StackSet as W
--------------------------------------------------------------------------------
-- Others
--------------------------------------------------------------------------------
import Control.Monad (liftM2)
import Data.List
import DBus.Client
import System.Taffybar.XMonadLog ( dbusLog )

--------------------------------------------------------------------------------
-- Main Xmonad
--------------------------------------------------------------------------------
main :: IO ()
main = do
  client <- connectSession
  taffy <- spawnPipe "~/.cabal/bin/taffybar"
--    _ <- spawnPipe "tint2 -c /home/eleazar/.config/tint2/tint2rc"
  _ <- spawnPipe "bash /home/eleazar/conky-manager/conky-startup.sh"
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"

  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 0
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , logHook            = ppXbar xmproc 
                           <+> (dbusLog client) <+> myLogHook
    , layoutHook         = showWName myLayouts
    , manageHook         = myManageHook 
                           <+> manageDocks 
                           <+> dynamicMasterHook 
                           <+> toggleHook "float" doFloat
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook
    }
      `additionalKeys` myKeys

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

startupWorkspace :: [Char]
startupWorkspace = "Hask"

myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook 

myFadeHook :: FadeHook
myFadeHook = composeAll [  opaque ,isUnfocused --> opacity 0.75
                           , className =? "mplayer2"  --> opaque
                        ]

myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D"
        startupHook defaultConfig
        spawnOnce myAppOnStartup
        windows $ W.greedyView startupWorkspace

        
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

--ppXbar :: Handle -> X ()
ppXbar bar = dynamicLogWithPP $ xmobarPP 
                { ppOutput = hPutStrLn bar
                , ppHiddenNoWindows = id
                , ppHidden = xmobarColor "green" ""
                , ppTitle = xmobarColor "white" "" . shorten 39
                , ppCurrent = xmobarColor "yellow" ""
                , ppOrder = (\(ws:l:t:xs) -> [t,l,ws])
                , ppSep = "   "}

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------
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



-- editorLayout = avoidStruts $ withIM (3/5) (Title "Yi") Grid

defaultLayouts = 
        named "Grid"      Grid
	||| named "Full"      Full 
        ||| named "Mirror"    (Mirror $ ResizableTall 1 (3/100) (1/2) [])
        ||| named "Magnifier" (magnifier $ ResizableTall 1 (3/100) (1/2) [])
        ||| named "Tabbed"     (tabbed shrinkText defaultTheme)
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

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings =
  [
    ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((myModMask,               xK_m), withFocused minimizeWindow) -- minimiza la app :)
    , ((myModMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    , ((myModMask .|. controlMask              , xK_plus ), sendMessage MagnifyMore) -- Magnifique Controls
    , ((myModMask .|. controlMask              , xK_minus), sendMessage MagnifyLess)
    , ((myModMask .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
    , ((myModMask .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
    , ((myModMask .|. controlMask              , xK_m    ), sendMessage Toggle     ) -- End magnifique controls
    , ((myModMask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig (\ws -> W.greedyView ws . W.shift ws)) -- display grid select and go to selected workspace
    , ((myModMask, xK_g), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((myModMask, xK_q), spawn "killall dzen2 ; killall conky ; killall tint2 ; killall taffybar-linux-x86_64; xmonad --recompile && xmonad --restart")
    , ((mod4Mask, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
    , ((myModMask, xK_f), spawn "synapse")
    , ((myModMask, xK_x), runOrRaisePrompt myXPConfig)
    , ((myModMask, xK_l), layoutPrompt myXPConfig)
    , ((myModMask .|. shiftMask, xK_x), changeDir myXPConfig)
    , ((myModMask, xK_t), withFocused $ windows . W.sink)
    , ((myModMask, xK_w), spawn "oblogout")
    , ((myModMask, xK_i), spawn "terminator -e screen irssi")
    , ((myModMask, xK_c), spawn "chromium-browser")
    , ((myModMask, xK_n), spawn "nautilus")
    , ((myModMask, xK_e), spawn myEditor)
    , ((myModMask, xK_u), focusUrgent) -- me redirige al foco urgente
    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 1%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 1%+")
  ]


-- TECLADO NUMERICO
numPadKeys :: [KeySym]
numPadKeys = [ xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
             , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
             , xK_KP_Left, xK_KP_Begin,xK_KP_Right
             , xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
             ]

numKeys :: [KeySym]
numKeys = [ xK_0, xK_minus, xK_equal
          , xK_1, xK_2, xK_3
          , xK_4, xK_5, xK_6
          , xK_7, xK_8, xK_9
          ]

myplaneKeys :: KeyMask -> Lines -> Limits -> [((KeyMask, KeySym), X ())]
myplaneKeys modm ln limits =
  [ ((keyMask, keySym), function ln limits direction)
  | (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft
  , (keyMask, function) <- [(modm, planeMove), (shiftMask .|. modm, planeShift)]
  ]

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = myKeyBindings ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++ myplaneKeys myModMask (Lines 4) Circular
