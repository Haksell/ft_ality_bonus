{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified SDL

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop renderer
  SDL.destroyWindow window

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents
  let isQuitEvent event =
        case SDL.eventPayload event of
          SDL.WindowClosedEvent _ -> True
          SDL.KeyboardEvent keyboardEvent ->
            case SDL.keyboardEventKeyMotion keyboardEvent of
              SDL.Pressed -> SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeEscape
              SDL.Released -> False
          _ -> False
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
  SDL.clear renderer
  SDL.present renderer
  unless (any isQuitEvent events) $ appLoop renderer
