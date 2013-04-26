------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
{-# LANGUAGE NoMonomorphismRestriction, ParallelListComp #-}
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
import XMonad.Util.EZConfig
import DBus.Client
import System.Taffybar.XMonadLog
import Control.Monad (liftM2)
import Data.Monoid

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- XMonad:
main :: IO ()
main = do
  client <- connectSession -- taffybar and dbus
  taff <- spawnPipe "taffybar"
  c1 <- spawnPipe "conky -c ~/.conky/.conkyrcmiui"
  c2 <- spawnPipe "conky -c ~/.conky/conky-calendar"
  c3 <- spawnPipe "conky -c ~/.conky/infoconky"

  xmonad $ ewmh $ defaultConfig
 
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
 
    , layoutHook         = myLayouts
    , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook <+> toggleHook "float" doFloat <+> manageHook defaultConfig
    , handleEventHook    = myHandleEventHook
    , logHook            = fadeWindowsLogHook myFadeHook <+> dbusLogWithPP client ppTaff 
    , startupHook        = myStartupHook
    }
      `additionalKeys` myKeys
 
myTerminal = "terminator"
myEditor = "terminator vim"
myBorderWidth = 0
myModMask = mod4Mask
-- Color, font and iconpath definitions:
myFont = "xft:terminus:size=10"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
 
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
ppTaff = defaultPP { ppHiddenNoWindows = taffybarEscape . const ""-- (\wsId -> if wsId == "5:Haskell" then "" else wsId)
                   , ppLayout = taffybarColor "red" "" .
                        (\x -> case x of
                            "Minimize Full"                                              -> "|Full|"
                            "Minimize Mirror ResizableTall"                              -> "|Mirr|"
                            "Minimize Magnifier ResizableTall"                           -> "|Magn|"
                            "Minimize layoutN Tabbed Simplest layoutAll Tabbed Simplest" -> "|Tabb|" 
                            _ -> x)
                   }


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
 
--myLayouts =
-- onWorkspace "Chat" chatLayout
-- $ defaultLayouts
--chatLayout = avoidStruts(withIM (1%7) (Title myIMRosterTitle) Grid)
-- Mis Layouts

myLayouts = avoidStruts $ minimize (
  ((layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) $ simpleTabbed)
  $ (layoutAll (relBox 0.5 0 1 1)                        $ simpleTabbed))

  ||| magnifier (ResizableTall 1 (3/100) (1/2) [])

  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  ||| noBorders Full)
 
-- Window rules:
myManageHook :: ManageHook
myManageHook = composeAll . concat $
             [ [isDialog --> doFloat]
             , [resource =? i --> doIgnore    | i <- myIgnores]]
             ++ [inWorksp doShiftAndGo w s    | w <- myWorkspaces | s <- myShifts ]
             ++ [inWorksp (const doFloat) () s | s <- myShifts]

         where doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
               inWorksp d w s = [(className =? x <||> title =? x <||> resource =? x) --> (d w) | x <- s]
               myShifts = [my1Shifts, my2Shifts, my3Shifts, my4Shifts, my5Shifts, my6Shifts, my7Shifts, my8Shifts, my9Shifts]
               myFloats = ["Plasma-desktop", "Plasma", "plasma-desktop", "oblogout", "kmix", "Downloads", "cairo-dock-unity-bridge", "Save As...","power-hs", "Diálogo de progreso", "Openbox Logout", "kmix"]
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
