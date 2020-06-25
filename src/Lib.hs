module Lib
    ( draw
    , basicScene
    , ScreenConfig (ScreenConfig)
    ) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word

data ScreenConfig = ScreenConfig { screenWidth :: Int
                                 , screenHeight :: Int
                                 }
data Scene = Scene { scenePoint :: Point
                   }
data Tri = Tri { t1 :: Point
               , t2 :: Point
               , t3 :: Point
               }
data Point = Point { px :: Float
                   , py :: Float
                   , pz :: Float
                   }

basicScene :: Scene
basicScene = Scene $ Point 200.0 300.0 10.0

draw :: ScreenConfig -> Scene -> ByteString
draw scfg scn = flatpack $ blankScreen scfg


type Screen = [[Pixel]]
data Pixel = Pixel { r :: Word8, g :: Word8, b :: Word8 }

blankScreen :: ScreenConfig -> Screen
blankScreen scfg = Prelude.replicate h (Prelude.replicate w blackPixel)
    where w = screenWidth scfg
          h = screenHeight scfg
          blackPixel = Pixel 0 255 0

flatpack :: Screen -> ByteString
flatpack scr = B.pack $ concatMap p2words $ concat scr
    where p2words (Pixel r g b) = [b, g, r, 255]