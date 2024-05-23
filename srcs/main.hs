{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Word (Word8)
import qualified SDL

color1 :: SDL.V4 Word8
color1 = SDL.V4 0 0 255 255

color2 :: SDL.V4 Word8
color2 = SDL.V4 255 0 0 255

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "ft_ality" SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 800 800}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop renderer color1
  SDL.destroyWindow window

appLoop :: SDL.Renderer -> SDL.V4 Word8 -> IO ()
appLoop renderer currentColor = do
  events <- SDL.pollEvents
  let color =
        if any colorToggleEvent events
          then if currentColor == color1 then color2 else color1
          else currentColor
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer
  SDL.present renderer
  unless (any isQuitEvent events) $ appLoop renderer color
 where
  getKeyPress :: SDL.KeyboardEventData -> SDL.Keycode
  getKeyPress keyboardEvent = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent

  isQuitEvent :: SDL.Event -> Bool
  isQuitEvent event =
    case SDL.eventPayload event of
      SDL.WindowClosedEvent _ -> True
      SDL.KeyboardEvent keyboardEvent ->
        case SDL.keyboardEventKeyMotion keyboardEvent of
          SDL.Pressed -> getKeyPress keyboardEvent == SDL.KeycodeEscape
          SDL.Released -> False
      _ -> False

  colorToggleEvent :: SDL.Event -> Bool
  colorToggleEvent event =
    case SDL.eventPayload event of
      SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
          && getKeyPress keyboardEvent /= SDL.KeycodeEscape
      _ -> False
