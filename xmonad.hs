------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
{-# LANGUAGE NoMonomorphismRestriction, ParallelListComp #-}
------------------------------------------------------------------------

import XMonad -- hiding (Tall)
--------------------------------------------------------------------------------
-- Actions imports
--------------------------------------------------------------------------------
import XMonad.Actions.GridSelect
import XMonad.Actions.Plane
--import qualified XMonad.Actions.Submap as SM
--import qualified XMonad.Actions.Search as S
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
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutBuilder -- construye contenedores
import XMonad.Layout.Magnifier -- incrementa la ventana enfocada
import XMonad.Layout.Tabbed
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize
--------------------------------------------------------------------------------
-- Others from Xmonad
--------------------------------------------------------------------------------
-- import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Man

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
--------------------------------------------------------------------------------
-- Taffybar imports
--------------------------------------------------------------------------------
import DBus.Client

import System.Taffybar.XMonadLog
import System.Taffybar.Hooks.PagerHints (pagerHints)
--------------------------------------------------------------------------------
-- Others
--------------------------------------------------------------------------------
import Control.Monad (liftM2)
import Data.Monoid
import Data.List
--import Network.SimpleIRC -- Add a manager IRC to xmonad


--------------------------------------------------------------------------------
-- Main Xmonad
--------------------------------------------------------------------------------
main :: IO ()
main = do
  client <- connectSession -- taffybar and dbus
  taff <- spawnPipe "~/.cabal/bin/taffybar"
  myStatus <- spawnPipe myTopStatusBar

  xmonad $ myUrgencyHook $ ewmh $ defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = False
    , borderWidth        = 0
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook            = fadeWindowsLogHook myFadeHook <+> (myLogHook myStatus) <+> ewmhDesktopsLogHook >> setWMName "LG3D" <+> dbusLogWithPP client ppTaff
    , layoutHook         = myLayouts
    , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook <+> toggleHook "float" doFloat
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook
    }
      `additionalKeys` myKeys

myTerminal, myEditor :: String
myTerminal = "terminator"
myEditor = "terminator -e yi"
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces =
  [
    "Any", "Not", "Nev",
    "Term", "Expl", "Game",
    "Docu", "Hask", "Web",
    "Chat", "Play", "Inks"
  ]


startupWorkspace = "Hask"

myTopStatusBar    = "dzen2 -x '0' -y '0' -h '20' -w '1366' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -p -e ''"

myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> minimizeEventHook 

myFadeHook = composeAll [  opaque ,isUnfocused --> opacity 0.75
                           , className =? "mplayer2"  --> opaque
                        ]
myStartupHook = do
        setWMName "LG3D"
        startupHook defaultConfig
        spawnOnce myAppOnStartup
        windows $ W.greedyView startupWorkspace
        
myAppOnStartup = (flip (++)) "&" . intercalate " &\n" $
      [ "xcompmgr",
        "clementine",
        "nm-applet",
        "guake",
        "synapse",
        "nautilus -n",
        "gnome-sound-applet",
        "/usr/bin/gnome-keyring-daemon --start --components=ssh,secrets,gpg,pkcs11",
        "/usr/libexec/gnome-settings-daemon"
        -- "taffybar -c=configfile -- TODO
      ] 

myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }


--myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppOutput          = hPutStrLn h
    --, ppSort            = ppSort defaultPP) -- hide "NSP" from workspace list
    , ppOrder           = orderText
    , ppExtras          = []
    , ppSep             = "^fg(" ++ colorGray ++ ")|"
    , ppWsSep           = ""
    , ppCurrent         = dzenColor colorYellow     colorBlack . pad
    , ppUrgent          = dzenColor colorGreen    colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppVisible         = dzenColor colorGray     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppHidden          = dzenColor colorWhiteAlt colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppHiddenNoWindows = dzenColor colorGray     colorBlack . pad . wrapClickWorkSpace . (\a -> (a,a))
    , ppLayout          = const ""
    , ppTitle           = dzenColor colorWhiteAlt colorBlack . pad . wrapClickTitle . titleText
. dzenEscape
    }
    where
        --display config
        orderText (ws:l:t:_) = [ws,l,t]
        titleText [] = "Desktop " ++ myArrow
        titleText x = (shorten 60 x) ++ " " ++ myArrow
        wrapClickLayout content = "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"                                                           --clickable layout
        wrapClickTitle content = "^ca(1,xdotool key super+j)" ++ content ++ "^ca()"                                                                --clickable title
        wrapClickWorkSpace (idx,str) = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "e;" ++ xdo index ++ ")" ++ str ++ "^ca()^ca()" --clickable workspaces
            where
                wsIdxToString Nothing = "1"
                wsIdxToString (Just n) = show (n+1)
                index = wsIdxToString (elemIndex idx myWorkspaces)
                xdo key = "xdotool key super+" ++ key


ppTaff :: PP
ppTaff = taffybarPP { ppHiddenNoWindows = taffybarEscape . const ""-- (\wsId -> if wsId == "5:Haskell" then "" else wsId)
                    , ppHidden = taffybarEscape . const ""
                    , ppTitle = taffybarEscape
                    , ppCurrent = taffybarColor "Black" ""
                    , ppOrder = (\(ws:l:t:xs) -> [l])
                    , ppLayout = taffybarColor "red" "" . id
                    }

--------------------------------------------------------------------------------
-- Colors and Fonts
--------------------------------------------------------------------------------
dzenFont             = "DejaVu Sans Mono:size=12"
myFont               = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
colorBlack           = "#1a1a1a" --Background (Dzen_BG)
colorBlackAlt        = "#404040" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#161616" --Gray dark
colorWhite           = "#ffffff" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#87afd7"
colorYellow          = "#ffaf5f"
colorRed             = "#d75f5f"
colorGreen           = "#87af5f"
myArrow              = "^fg(" ++ colorRed ++ ")>->"
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#d75f5f"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------
--myLayouts =
-- onWorkspace "Chat" chatLayout
-- $ defaultLayouts
--chatLayout = avoidStruts(withIM (1%7) (Title myIMRosterTitle) Grid)

myLayouts = named "Grid"      ( avoidStruts $ minimize Grid )
	||| named "Full"      ( avoidStruts $ minimize Full )
        ||| named "Builder"   (avoidStruts $ minimize (((layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) $ simpleTabbed)
                               (layoutAll (relBox 0.5 0 1 1) simpleTabbed))))
        ||| named "Magnifier" (minimize $ magnifier (ResizableTall 1 (3/100) (1/2) []))
        ||| named "Float"     (minimize $ floatingDeco $ borderResize $ positionStoreFloat)
        where named x = renamed [Replace x]
              floatingDeco = noFrillsDeco shrinkText defaultTheme
        ---  ||| Mirror (ResizableTall 1 (3/100) (1/2) []))


-- Window rules:
myManageHook :: ManageHook
myManageHook = composeAll . concat $
             [ [isDialog --> doFloat]
             , [resource =? i --> doIgnore    | i <- myIgnores]]
             ++ [inWorksp doShiftAndGo w s    | w <- myWorkspaces | s <- myShifts ]
             ++ [inWorksp (const doFloat) () myFloats]

         where doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
               inWorksp d w s = [(className =? x <||> title =? x <||> resource =? x) --> (d w) | x <- s]
               myShifts = [[], [], [], my1Shifts, my2Shifts, my3Shifts, my4Shifts, my5Shifts, my6Shifts, my7Shifts, my8Shifts, my9Shifts]
               myFloats = ["Notas adhesivas", "Guake", "guake.py", "Escritorio"]
               myIgnores = []
               my1Shifts = [myTerminal]
               my2Shifts = ["nautilus", "ranger", "dolphin"]
               my3Shifts = []
               my4Shifts = ["evince"]
               my5Shifts = ["gedit", "emacs24","SublimeText", "yi", "gvim", "leksah"]
               my6Shifts = ["chromium-browser"]
               my7Shifts = ["emphaty","quassel", "thunderbird"]
               my8Shifts = ["clementine"]
               my9Shifts = ["inkscape"]

--------------------------------------------------------------------------------
-- Keybinding
--------------------------------------------------------------------------------
-- GSConfig options:
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_font = "" ++ myFont ++ ""
    }

-- XPConfig options:
myXPConfig = defaultXPConfig
    { bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 3
    , position = Top
    , height = 30
    , historySize = 30
    }

{-
configFile =  fromList $
      [ ((0, xK_x), spawn $ myEditor ++ "/home/elediaz/.xmonad/xmonad.hs")
      , ((0, xK_e), spawn myEditor)
      ]

searchEngineMap method = fromList $
      [ ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_w), method S.wikipedia)
      ]
-}
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
    --, ((myModMask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig (\ws -> W.greedyView ws . W.shift ws)) -- display grid select and go to selected workspace
    , ((myModMask, xK_g), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((myModMask, xK_q), spawn "killall dzen2 ; killall conky ; killall tint2 ; killall taffybar-linux-x86_64; xmonad --recompile && xmonad --restart")
    , ((mod4Mask, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
    , ((myModMask, xK_f), spawn "synapse")
    , ((myModMask, xK_x), xmonadPrompt myXPConfig)
    , ((myModMask, xK_F1), manPrompt myXPConfig)
    --, ((myModMask, xK_s), SM.submap $ searchEngineMap $ S.promptSearch defaultXPConfig)
    --, ((myModMask .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch) -- busca el texto selecionado
    --, ((myModMask .|. shiftMask, xK_e), SM.submap $ configFile)
    , ((myModMask, xK_e), toggleHookNext "float")
    , ((myModMask, xK_r), toggleHookAllNew "float")
    , ((myModMask, xK_t), withFocused $ windows . W.sink)
    , ((myModMask, xK_w), spawn "oblogout")
    , ((myModMask, xK_i), spawn "inkscape")
    , ((myModMask, xK_c), spawn "chromium-browser")
    , ((myModMask, xK_n), spawn "nautilus")
    , ((myModMask, xK_u), focusUrgent) -- me redirige al foco urgente
    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 1%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 1%+")
  ]


-- TECLADO NUMERICO
numPadKeys =
  [
    xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
  , xK_KP_Left, xK_KP_Begin,xK_KP_Right
  , xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
  ]

numKeys =
  [
    xK_0, xK_minus, xK_equal
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
