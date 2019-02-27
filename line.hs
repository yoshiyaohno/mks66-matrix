module Line where

import qualified Data.Set        as S
import qualified Data.List       as L
import qualified Data.Map.Strict as M

data Line   = Line Point Point deriving (Show)
data Point  = Point {getX::Int, getY::Int, getZ::Int, getQ::Int}
                deriving (Eq, Ord)
data Color  = Color {r::Int, g::Int, b::Int}

type Screen = M.Map Point Color
type DrawAction = Screen -> Screen

blk = Color 0 0 0
red = Color 255 0 0
blu = Color 0 0 255
grn = Color 0 255 0

-- Prelude.pi is a Floating and not a Fractional and that's stupid
_pi = 3.14159

instance Show Point where
    show (Point x y z q) = "(" ++ show x ++ ", " ++ show y ++ ")"
-- the duality of man
instance Show Color where
    show = unwords . map show . ([r, g, b] <*>) . pure

ppmHeader :: (Int, Int) -> String
ppmHeader (w, h) = "P3 " ++ show w ++ " " ++ show h ++ " 255\n"

-- all (non-permuted) pairs of a list
allPairs :: [a] -> [(a, a)]
allPairs []     = []
allPairs (_:[]) = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs
-- look at me actually writing comments

--main = do
--    let points  = polygon 60 255 (Point 470 470)
--        lines   = [drawLine (Line p0 p1)
--                            (Color  ((getX p0 + getX p1) `div` 4)
--                                    ((getY p0 + getY p1) `div` 4)
--                                    ((getX p0 + getY p0) `div` 2))
--                    | (p0, p1) <- allPairs points]
--        drawing = foldr ($) M.empty lines
--    writeFile "out.ppm" (printPixels (900, 900) drawing)

-- floating point math :'(
polygon :: (Integral a) => a -> a -> Point -> [Point]
polygon s r (Point x y z q) =
    [Point  (x + round (fromIntegral r * sin ((fromIntegral i) * th)))
            (y + round (fromIntegral r * cos ((fromIntegral i) * th)))
            0 1
        | i <- [0..s-1]]
    where th = (2 * _pi) / (fromIntegral s)
-- cmon haskell I'm just trying to multiply a sine why do you have to make
--  the typing so hard

-- takes bounds and a screen and puts in ppm format
printPixels :: (Int, Int) -> Screen -> String
printPixels (w, h) pxs =
    ppmHeader (w, h)
    ++ (unlines . map unwords $ [[show . f $ M.lookup (Point x y 0 1) pxs
                | x <- [0..w-1]] | y <- [0..h-1]])
    where   f Nothing  = Color 0 0 0
            f (Just c) = c 

-- wait it's getting better
    -- mmmm we r gonna have to change this one a whole lot
drawLine :: Line -> Color -> DrawAction
drawLine l c scrn = foldr insC scrn (rasterLine l)
    where insC = flip M.insert c

-- just gives you the points a line covers, no color
rasterLine :: Line -> [Point]
rasterLine (Line p0 p1)
    | dy == 0 && dx == 0        = []
    | abs dx > abs dy && dx > 0 = _rLx (Line p0 p1)
    | abs dx > abs dy           = _rLx (Line p1 p0)
    | dy > 0                    = _rLy (Line p0 p1)
    | otherwise                 = _rLy (Line p1 p0)
    where   dy = (getY p1) - (getY p0) 
            dx = (getX p1) - (getX p0)

-- hell yeah ugly helper functions
--  (I could use only one by flipping the tuples somehow but ergh)
_rLx :: Line -> [Point]
_rLx (Line (Point x0 y0 _ _) (Point x1 y1 _ _)) =
    L.zipWith4 (Point) [x0..x1] ys (repeat 0) (repeat 1)
    where   ys = map ((+y0) . (`quot` (2* abs dx))) . tail $ [negate dy, dy..]
            dy = y1 - y0
            dx = x1 - x0

_rLy :: Line -> [Point]
_rLy (Line (Point x0 y0 _ _) (Point x1 y1 _ _)) =
    L.zipWith4 (Point) xs [y0..y1] (repeat 0) (repeat 1)
    where   xs = map ((+x0) . (`quot` (2* abs dy))) . tail $ [negate dx, dx..]
            dy = y1 - y0
            dx = x1 - x0
