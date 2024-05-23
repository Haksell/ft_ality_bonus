{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let isQuitEvent event =
        case eventPayload event of
          WindowClosedEvent _ -> True
          KeyboardEvent keyboardEvent ->
            case keyboardEventKeyMotion keyboardEvent of
              Pressed -> keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
              Released -> False
          _ -> False
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless (any isQuitEvent events) $ appLoop renderer
