------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad -- hiding (Tall)
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Minimize
import XMonad.Hooks.ToggleHook
import XMonad.Layout.Minimize
--import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutBuilder -- construye contenedores
import XMonad.Layout.Magnifier -- incrementa la ventana enfocada
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Man
import XMonad.Util.Run
--import XMonad.Util.Themes
import XMonad.Util.EZConfig
import DBus.Client
import System.Taffybar.XMonadLog
import Control.Monad (liftM2)
import Data.Monoid

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- XMonad:
main = do
  client <- connectSession
  --let pp = defaultPP
  taff <- spawnPipe "taffybar"
  --dzen <- spawnPipe myStatusBar
  --tint <- spawnPipe "trayion"
  --dzenRightBar <- spawnPipe myStatusOther
  --trayicon <- spawnPipe "tint2 -c ~/.conky/black_transparency/tint2/tint2rc" --myTrayIcon
  c1 <- spawnPipe "conky -c ~/.conky/.conkyrcmiui"
  c2 <- spawnPipe "conky -c ~/.conky/conky-calendar"
  c3 <- spawnPipe "conky -c ~/.conky/infoconky"
  --toDo <- spawnPipe myToDo

  xmonad $ ewmh $ myUrgencyHook $ defaultConfig
 
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
 
    --, keys               = myKeys
    --, mouseBindings      = myMouseBindings
 
    , layoutHook         = myLayouts
    , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook <+> toggleHook "float" doFloat <+> manageHook defaultConfig
    , handleEventHook    = myHandleEventHook
    , logHook            = fadeWindowsLogHook myFadeHook <+> dbusLogWithPP client ppTaff -- <+> dynamicLogWithPP (myDzenPP dzen)
    , startupHook        = myStartupHook
    }
      `additionalKeys` myKeys
 
myTerminal = "terminator"
myEditor = "SublimeText"
myFocusFollowsMouse = False
myBorderWidth = 0
myModMask = mod4Mask

myWorkspaces =
  [
    "7:Chat", "8:Player", "9:Inkscape",
    "4:Documents", "5:Haskell", "6:Web",
    "1:Terminal", "2:Explore", "3:Game"
  ]

myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#1f1f1f"
 
startupWorkspace = "5:Haskell"

myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook

myFadeHook = composeAll [  opaque ,isUnfocused --> opacity 0.75
                           , className =? "mplayer2"  --> opaque
                        ]
myEventHook = mempty
myStartupHook = do
        setWMName "LG3D"
        windows $ W.greedyView startupWorkspace
        spawn "~/.xmonad/startup-hook"

ppTaff :: PP 
ppTaff = defaultPP { ppHiddenNoWindows = taffybarEscape . (\_ -> "")-- (\wsId -> if wsId == "5:Haskell" then "" else wsId)
                   , ppLayout = taffybarColor "red" "" .
                        (\x -> case x of
                            "Minimize Full"                                              -> "|Full|"
                            "Minimize Mirror ResizableTall"                              -> "|Mirr|"
                            "Minimize Magnifier ResizableTall"                           -> "|Magn|"
                            "Minimize layoutN Tabbed Simplest layoutAll Tabbed Simplest" -> "|Tabb|" 
                            _ -> x)
                   }

-- Color, font and iconpath definitions:
myFont = "xft:terminus:size=10"
myIconDir = "/home/elediaz/.xmonad/Icons"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
myPatternColor = "#1f1f1f"
mySeperatorColor = "#555555"
 
-- GSConfig options:
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    , gs_font = "" ++ myFont ++ ""
    }
 
-- XPConfig options:
myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 3
    , position = Bottom
    , height = 30
    , historySize = 100
    }
 
-- Theme options:
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    }
 
-- Statusbar options:
myConky2, myToDo, myConky :: [Char]
myConky2 = "conky -c ~/.conky/conkyrc_red"
myToDo = "conky -c ~/.conky/_.conkyrc2"
myConky = "conky -c ~/.conky/conkyForecast.template"
myTrayIcon = "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand false --width 10 --transparent false --tint 0xa9a9a9 --height 11" -- quitar SetDockType para poder minimizar
myStatusOther = "conky -c /home/elediaz/.xmonad/.conky_dzen | dzen2 -x '0' -y '500' -w '1366' -h '16' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF'"
myStatusBar = "dzen2 -x '0' -y '752' -h '16' -w '1390' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"


-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "0", "-h", "25", "-w", "1390", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }
 
--myLayouts =
-- onWorkspace "Chat" chatLayout
-- $ defaultLayouts
--chatLayout = avoidStruts(withIM (1%7) (Title myIMRosterTitle) Grid)
-- Mis Layouts

myLayouts = avoidStruts $ minimize (
  ((layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) $ simpleTabbed)
  $ (layoutAll (relBox 0.5 0 1 1)                         $ simpleTabbed))

  ||| magnifier (ResizableTall 1 (3/100) (1/2) [])

  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  ||| noBorders Full)



 
-- Window rules:
myManageHook = composeAll . concat $ -- the magic shell comand xprop -- for see properties of windows
    [ [isDialog --> doFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [className =? "Cairo-dock" --> doIgnore]
    , [isFullscreen --> doFullFloat]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "1:Terminal"  | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "2:Explore"   | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "3:Game"      | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "4:Documents" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5:Haskell"   | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6:Web"       | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:Chat"      | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "8:Player"    | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "9:Inkscape"  | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Plasma-desktop", "Plasma", "plasma-desktop", "oblogout", "kmix"]
    myTFloats = ["Downloads", "cairo-dock-unity-bridge", "Save As...","power-hs", "Diálogo de progreso", "Openbox Logout", "kmix"]
    myRFloats = []
    myIgnores = []
    my1Shifts = [myTerminal]
    my2Shifts = ["dolphin", "ranger"]
    my3Shifts = []
    my4Shifts = []
    my5Shifts = ["qvim", "kate", "geany","SublimeText"]
    my6Shifts = ["chromium-browser", "rekonq","chrome-stable-25.0.1364.160-1"]
    my7Shifts = ["emphaty","quassel", "thunderbird"]
    my8Shifts = ["clementine"]
    my9Shifts = ["inkscape"]
 
-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/arch_10x10.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/arch_10x10.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/arch_10x10.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/arch_10x10.xbm)") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/arch_10x10.xbm)^fg(" ++ myUrgentFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = "|"
    , ppWsSep = "|"
    , ppTitle = dzenColor "#00aa4a" "" . wrap ">>=" "=<<"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Minimize Full" -> "^fg(" ++ myUrgentFGColor ++ ")^i(" ++ myIconDir ++ "/stop.xbm)"
        "Minimize Mirror ResizableTall" -> "^fg(" ++ myUrgentFGColor ++ ")^i(" ++ myIconDir ++ "/fs_01.xbm)"
        "Minimize Tabbed Simplest" -> "^fg(" ++ myUrgentFGColor ++ ")^i(" ++ myIconDir ++ "/pacman.xbm)"
        "Minimize Magnifier ResizableTall" -> "^fg(" ++ myUrgentFGColor ++ ")^i(" ++ myIconDir ++ "/mem.xbm)"
        "Minimize layoutN Tabbed Simplest layoutAll Tabbed Simplest" -> "^fg(" ++ myUrgentFGColor ++ ")^i(" ++ myIconDir ++ "/pause.xbm)"
        "Minimize Grid" -> "^fg(" ++ myUrgentFGColor ++ ")^i(" ++ myIconDir ++ "/shroom.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if ':' `elem` wsId then drop 2 wsId else wsId
    staticWs = ["5:Haskell"]

configFile =  M.fromList $
      [ ((0, xK_x), spawn $ myEditor ++ "/home/elediaz/.xmonad/xmonad.hs")
      , ((0, xK_e), spawn myEditor)
      ]

searchEngineMap method = M.fromList $
      [ ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_w), method S.wikipedia)
      ]

myKeyBindings =
  [
    ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((myModMask .|. shiftMask, xK_h ), sendMessage $ IncLayoutN (-1)) -- LayoutBuilder
    , ((myModMask .|. shiftMask, xK_l ), sendMessage $ IncLayoutN 1) -- LayoutBuilder
    , ((myModMask,               xK_m), withFocused minimizeWindow) -- minimiza la app :)
    , ((myModMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
    , ((myModMask .|. controlMask              , xK_plus ), sendMessage MagnifyMore) -- Magnifique Controls
    , ((myModMask .|. controlMask              , xK_minus), sendMessage MagnifyLess)
    , ((myModMask .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
    , ((myModMask .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
    , ((myModMask .|. controlMask              , xK_m    ), sendMessage Toggle     ) -- End magnifique controls
    , ((myModMask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspace
    , ((myModMask, xK_g), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((myModMask .|. shiftMask, xK_Tab), windows W.focusUp) -- move focus to the previous window
    , ((myModMask, xK_r), spawn "~/.xmonad/startup-hook") -- innecesario
    , ((myModMask, xK_q), spawn "killall dzen2 ; killall conky ; killall tint2 ; killall taffybar-linux-x86_64; xmonad --recompile && xmonad --restart")
    --, ((myModMask, xK_f), spawn "krunner")
    , ((mod4Mask, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
    , ((myModMask, xK_f), shellPrompt myXPConfig)
    
    , ((myModMask, xK_x), xmonadPrompt myXPConfig)
    , ((myModMask, xK_F1), manPrompt myXPConfig)
    , ((myModMask, xK_s), SM.submap $ searchEngineMap $ S.promptSearch defaultXPConfig)
    , ((myModMask .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch) -- busca el texto selecionado 
    , ((myModMask .|. shiftMask, xK_e), SM.submap $ configFile)  
    , ((myModMask, xK_e), toggleHookNext "float")
    , ((myModMask, xK_r), toggleHookAllNew "float")
    , ((myModMask, xK_t), withFocused $ windows . W.sink)
    , ((myModMask, xK_w), spawn "oblogout")
    , ((mod4Mask, xK_i), spawn "inkscape")
    , ((myModMask, xK_c), spawn "chrome-stable-25.0.1364.160-1") 
    , ((myModMask, xK_u), focusUrgent) -- ???
    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 1%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 1%+")
  ]


-- TECLADO NUMERICO
numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
  ]

numKeys =
  [
    xK_7, xK_8, xK_9
    , xK_4, xK_5, xK_6
    , xK_1, xK_2, xK_3
  ]

-- Here, some magic occurs that I once grokked but has since
-- fallen out of my head. Essentially what is happening is
-- that we are telling xmonad how to navigate workspaces,
-- how to send windows to different workspaces,
-- and what keys to use to change which monitor is focused.
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
  ] 
