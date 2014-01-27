--------------------------------------------------------------------------------
-- Module     : Kashe
-- Copyright   : Eleazar Díaz Delgado
-- License    : MIT
--
-- Maintainer  : Eleazar Díaz Delgado
-- Stability   : UnStable
-- Portability : None
--
-- Litle utility for desktop, show hour and trayicon
--
--------------------------------------------------------------------------------
--
--module Kashe.Kashe where
--
module Main (main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Misc.TrayManager
import Graphics.Rendering.Cairo
import Control.Concurrent ( forkIO, threadDelay )
import Control.Exception as E
import Control.Monad ( forever )
import Control.Monad.IO.Class
import Data.Time
import System.Locale

-- cairo :: Render ()
cairo = do
  win <- eventWindow
  liftIO $ do
  (width',height') <- drawableGetSize win
  let width  = realToFrac width'
      height = realToFrac height'

  -- Draw using the cairo api
  renderWithDrawable win $ do
    setSourceRGB 1 0 0
    setLineWidth 20
    setLineCap LineCapRound
    setLineJoin LineJoinRound

    moveTo 30 30
    lineTo (width-30) (height-30)
    lineTo (width-30) 30
    lineTo 30 (height-30)
    stroke

    setSourceRGB 1 1 0
    setLineWidth 4

  return True


main :: IO ()
main = do
    initGUI
    window <- windowNew
    windowSetDecorated window False
    windowSetResizable window True
    windowSetPosition window WinPosCenterAlways
    windowSetTitle window "Kashe"
    -- windowSetDefaultSize window 500 200

    -- draw <- drawWindowGetDefaultRootWindow
    -- window `on` exposeEvent $ cairo

    base <- vBoxNew True 0
    box <- hBoxNew False 5

    lal <- labelNew Nothing
    _ <- on lal realize $ do
          _ <- forkIO $ forever $ do
                         let tryUpdate = do
                                   time <- getCurrentTime
                                   postGUIAsync $ labelSetMarkup lal (getTime time)
                         E.catch tryUpdate ignoreIOException
                         threadDelay 1000000
          return ()
    boxPackStart base lal PackNatural 0

    Just screen <- screenGetDefault
    tray <- trayManagerNew
    _ <- trayManagerManageScreen tray screen
    _ <- on tray trayIconAdded $ \w -> widgetShowAll w >> boxPackStart box w PackGrow 5
    on window deleteEvent (liftIO $ mainQuit >> return True)
    boxPackStart base box PackNatural 0
    containerAdd window base

    widgetShowAll window
    mainGUI


getTime :: FormatTime t => t -> String
getTime t = "<span foreground=\"black\" size=\"36800\" font_weight=\"bold\">" ++ (formatTime defaultTimeLocale "%T" t) ++ "</span>"

ignoreIOException :: IOException -> IO ()
ignoreIOException = print
