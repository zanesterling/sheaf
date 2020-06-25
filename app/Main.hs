{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Control.Monad
import qualified SDL
import SDL.Vect
import SDL.Video.Renderer

main :: IO ()
main = do
    SDL.initializeAll

    let (w, h) = (640, 480)
        dim = V2 w h
        range = w * 4
    let winConfig = SDL.defaultWindow { SDL.windowInitialSize = dim }
        rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                       , SDL.rendererTargetTexture = True
                                       }

    window <- SDL.createWindow "Hello World!" winConfig
    renderer <- SDL.createRenderer window (-1) rdrConfig
    texture <- createTexture renderer ARGB8888 TextureAccessStreaming dim

    let loop = do
          SDL.clear renderer
          SDL.updateTexture texture Nothing (draw (ScreenConfig (fromIntegral w) (fromIntegral h))
                                                  basicScene) range
          SDL.copy renderer texture Nothing Nothing
          SDL.present renderer

          SDL.delay 20

          quit <- fmap (\ev -> case SDL.eventPayload ev of
              SDL.QuitEvent -> True
              SDL.KeyboardEvent e -> SDL.keyboardEventKeyMotion e == SDL.Pressed
              _ -> False) SDL.waitEvent
          unless quit loop

    loop

    SDL.destroyTexture texture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    
    SDL.quit
