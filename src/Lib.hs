{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( draw
    , update
    , basicScene
    , loadScene
    , loadScene'
    , ScreenConfig (ScreenConfig)
    ) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Word
import GHC.Float
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Text.Parsec (many, ParseError)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

f2i = float2Int
i2f = int2Float


--- SCENE ---
data Scene = Scene { scenePoints    :: [Point]
                   , sceneLines     :: [Line]
                   , sceneTriangles :: [Triangle]
                   }
data Triangle = Triangle { t1 :: Point
                         , t2 :: Point
                         , t3 :: Point
                         }
data Point = Point { px :: Float
                   , py :: Float
                   , pz :: Float
                   , pcolor :: Color
                   }
type Line = (Point, Point)

loadScene :: String -> IO (Either String Scene)
loadScene fn = mapLeft show <$> parseFromFile parseScene fn
    where mapLeft :: (a -> b) -> Either a r -> Either b r
          mapLeft f (Left a) = Left $ f a
          mapLeft f (Right r) = Right r

-- Parse a scene from the selected file and throw an error on failure.
loadScene' :: String -> IO Scene
loadScene' fn = do
    scn <- parseFromFile parseScene fn
    case scn of
        Left err -> error $ show err
        Right scn -> return scn

parseScene :: Parser Scene
parseScene = do
    points     <- many (parsePoint <* many endOfLine)
    lines      <- many (parseLine <* many endOfLine)
    triangles  <- many (parseTriangle <* many endOfLine)
    eof
    return $ Scene points lines triangles

whitespace = many1 space

parsePoint :: Parser Point
parsePoint = do
    char 'P'
    x <- whitespace >> float
    y <- whitespace >> float
    z <- whitespace >> float
    c <- whitespace >> parseColor
    endOfLine
    return $ Point x y z c
parseColor :: Parser Color
parseColor = do
    char 'C'
    r <- whitespace >> integer
    g <- whitespace >> integer
    b <- whitespace >> integer
    return $ Pixel r g b

parseLine :: Parser Line
parseLine = do
    char 'L' >> endOfLine
    (,) <$> parsePoint <*> parsePoint

parseTriangle :: Parser Triangle
parseTriangle = do
    char 'T' >> endOfLine
    Triangle <$> parsePoint <*> parsePoint <*> parsePoint


integer :: Read i => Integral i => Parser i
integer = read <$> many1 digit
float :: Parser Float
float = do
    ipart <- many1 digit 
    fpart <- option "0" (char '.' >> many1 digit)
    return $ read (ipart ++ "." ++ fpart)

basicScene :: Scene
basicScene = Scene points lines triangles
    where
        points = map (\x -> pt2d x 400 white) [100, 150 .. 350]
        lines = [ (pt2d 100 100 red, pt2d 200 120 white)
                , (pt2d 100 100 red, pt2d 200  80 white)
                , (pt2d 100 100 red, pt2d 000 120 white)
                , (pt2d 100 100 red, pt2d 000  80 white)
                , (pt2d 100 100 red, pt2d 120 200 white)
                , (pt2d 100 100 red, pt2d 120 000 white)
                , (pt2d 100 100 red, pt2d  80 200 white)
                , (pt2d 100 100 red, pt2d  80 000 white)
                ]
        triangles = [ Triangle (pt2d 400 100 red) (pt2d 450  50 white) (pt2d 470 150 blue)
                    , Triangle (pt2d 400 200 red) (pt2d 450 150 white) (pt2d 470 200 blue)
                    , Triangle (pt2d 400 250 red) (pt2d 420 300 white) (pt2d 470 250 blue)
                    , Triangle (pt2d 400 300 red) (pt2d 420 400 white) (pt2d 470 350 blue)
                    ]
        pt2d x y = Point (i2f x) (i2f y) 0.0
        red = Pixel 255 0 0
        white = Pixel 255 255 255
        blue = Pixel 0 0 255

update :: Scene -> Scene
update scene = scene

--- DRAW ---

data ScreenConfig = ScreenConfig { screenWidth :: Int
                                 , screenHeight :: Int
                                 }
data Pixel = Pixel { r :: Word8, g :: Word8, b :: Word8 }
type Color = Pixel

draw :: ScreenConfig -> Scene -> ByteString
draw scfg scn = B.pack $ concatMap p2words $ runST $ do
    let (w, h) = (screenWidth scfg, screenHeight scfg)
    screen <- newArray (0, w * h) (Pixel 0 0 0) :: ST s (STArray s Int Pixel)

    let setPixel x y = writeArray screen (x + w * y)
    let setPixel' fx fy = setPixel (f2i fx) (f2i fy)

    forM_ (scenePoints scn) $ \(Point fx fy _ col) -> setPixel' fx fy col

    forM_ (sceneLines scn) (drawLine setPixel)
    forM_ (sceneTriangles scn) (drawTriangle setPixel)

    getElems screen

    where p2words (Pixel r g b) = [b, g, r, 255] -- convert to BGRA8888

type Pen s = Int -> Int -> Color -> ST s ()


drawPoint :: Pen s -> Point -> ST s ()
drawPoint setpx (Point x y _ col) = setpx (f2i x) (f2i y) col


drawLine :: Pen s -> Line -> ST s ()
drawLine setPixel (Point fx1 fy1 _ c1, Point fx2 fy2 _ c2) = do
    let (x1, y1, x2, y2) = (f2i fx1, f2i fy1, f2i fx2, f2i fy2)
    let (dx, dy) = (abs (x2 - x1), abs (y2 - y1))
    let (x1', y1', x2', y2') =
            if dx >= dy then (x1, y1, x2, y2) else (y1, x1, y2, x2)
    let (x1'', y1'', c1', x2'', y2'', c2') =
            if x1' <= x2' then (x1', y1', c1, x2', y2', c2) else (x2', y2', c2, x1', y1', c1)
    let setpx = if dx >= dy then setPixel else flip setPixel
        p1' = (x1'', y1'', c1')
        p2' = (x2'', y2'', c2')
    bresenham setpx p1' p2'

bresenham :: Pen s -> (Int, Int, Color) -> (Int, Int, Color) -> ST s ()
bresenham setpx (x1, y1, c1) (x2, y2, c2) =
    forM_ [x1 .. x2] $ \x ->
        let dx = i2f $ x - x1
            p = dx / i2f (x2 - x1)
            f = i2f (y2 - y1) / i2f (x2 - x1)
        in setpx x (y1 + f2i (f * dx)) (lerp c1 c2 p)

drawTriangle :: Pen s -> Triangle -> ST s ()
drawTriangle p (Triangle p1 p2 p3) = do
    let [top, mid, bot] = sortUnder py [p1, p2, p3]
        m = (py mid - py top) / (py bot - py top)
        mid' = lerp top bot m
    fillFlatTriangle p top mid mid'
    fillFlatTriangle p mid mid' bot

-- TODO: Seems to be a rounding error glitch with the right side of flat-topped
-- triangles. There are cut-in tooth marks.
fillFlatTriangle :: Pen s -> Point -> Point -> Point -> ST s ()
fillFlatTriangle p t m b
    | py t == py m && py m == py b =
        return ()
    | py m == py b =
        let [bl, br] = sortUnder px [m, b]
        in fillTrapezoid p (t, t) (bl, br)
    | otherwise =
        let [tl, tr] = sortUnder px [t, m]
        in fillTrapezoid p (tl, tr) (b, b)

fillTrapezoid :: Pen s -> (Point, Point) -> (Point, Point) -> ST s ()
fillTrapezoid setpx (l1, r1) (l2, r2) =
    forM_ [py l1 .. py l2] $ \y ->
        let m = (y - py l1) / (py l2 - py l1)
            xl = lerp (px l1) (px l2) m
            xr = lerp (px r1) (px r2) m
            cl = lerp (pcolor l1) (pcolor l2) m
            cr = lerp (pcolor r1) (pcolor r2) m
        in fillRow y xl xr cl cr
    where
        fillRow y x1 x2 c1 c2 = do
            setpx (f2i x1) (f2i y) (lerp c1 c2 0.5)
            forM_ [x1 .. x2] $ \x ->
             let m = (x - x1) / (x2 - x1)
             in setpx (f2i x) (f2i y) (lerp c1 c2 m)

sortUnder :: Ord b => (a -> b) -> [a] -> [a]
sortUnder f = sortBy (\a b -> f a `compare` f b)

-- WANT: ByteString constructor that takes an array and a (e -> [Word8]).

class Lerpy l where lerp :: l -> l -> Float -> l

w2f = i2f . fromIntegral
f2w = fromIntegral . f2i

instance Lerpy Float where lerp x y p = x*(1-p) + y*p
instance Lerpy Word8 where lerp x y p = f2w $ lerp (w2f x) (w2f y) p
instance Lerpy Pixel where
    lerp (Pixel r1 g1 b1) (Pixel r2 g2 b2) p =
        Pixel (lerp r1 r2 p) (lerp g1 g2 p) (lerp b1 b2 p)
instance Lerpy Point where
    lerp (Point x1 y1 z1 c1) (Point x2 y2 z2 c2) p =
        Point (lerp x1 x2 p) (lerp y1 y2 p) (lerp z1 z2 p) (lerp c1 c2 p)