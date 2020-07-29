{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Lib

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import qualified SDL
import SDL.Vect
import SDL.Video.Renderer
import System.FSNotify
import Data.Maybe
import System.IO

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

    let sceneFile = "assets/Basic.scn"
    basicScene <- loadScene' sceneFile
    sceneVar <- newEmptyMVar

    let loop scene = do
          mscene <- tryTakeMVar sceneVar
          let scene' = fromMaybe scene mscene

          SDL.clear renderer
          SDL.updateTexture texture Nothing (draw (ScreenConfig (fromIntegral w) (fromIntegral h)) scene') range
          SDL.copy renderer texture Nothing Nothing
          SDL.present renderer

          -- TODO: make this a more accurate frame limiter (don't waste 20ms when we don't have 20 to spare)
          SDL.delay 20

          quit <- flip fmap SDL.pollEvent $ \case
                Just ev ->
                    case SDL.eventPayload ev of
                        SDL.QuitEvent -> True
                        SDL.KeyboardEvent e -> SDL.keyboardEventKeyMotion e == SDL.Pressed
                        _ -> False
                Nothing -> False
          unless quit $ loop $ update scene'

    withManager $ \mgr -> do
        watchTree mgr "." (const True) $ \action ->
            --unless (eventPath action /= sceneFile) $
            loadScene sceneFile >>= \case
                Left _      -> return ()
                Right scene -> putMVar sceneVar scene
        loop basicScene

    SDL.destroyTexture texture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    
    SDL.quit
