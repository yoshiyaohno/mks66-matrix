module Line where

import           Control.Applicative
import qualified Data.Set        as S
import qualified Data.List       as L
import qualified Data.Map.Strict as M

data Line = Line (Vect Int) (Vect Int) deriving (Show)
data Vect a = Vect { getX::a
                   , getY::a
                   , getZ::a
                   , getQ::a
                   } deriving (Eq, Ord)
data Color  = Color {r::Int, g::Int, b::Int}

type Screen = M.Map (Int, Int) Color
type DrawAction = Screen -> Screen
type Transform a = Vect (Vect a)

blk = Color 0 0 0
red = Color 255 0 0
blu = Color 0 0 255
grn = Color 0 255 0

-- Prelude.pi is a Floating and not a Fractional and that's stupid
_pi = 3.14159

instance (Show t) => Show (Vect t) where
    show (Vect x y z q) = "("
                            ++ show x ++ ", "
                            ++ show y ++ ", "
                            ++ show z ++ ", "
                            ++ show q
                            ++ ")"

-- the duality of man
instance Show Color where
    show = unwords . map show . ([r, g, b] <*>) . pure

instance Functor Vect where
    fmap f (Vect x y z q) = (Vect (f x) (f y) (f z) (f q))
    
instance Applicative Vect where
    pure x = (Vect x x x x)
    (Vect f0 f1 f2 f3) <*> (Vect x y z q) =
        (Vect (f0 x) (f1 y) (f2 z) (f3 q))

instance Foldable Vect where
    foldr f acc (Vect x0 x1 x2 x3) =
        foldr f acc [x0, x1, x2, x3]

main = do
    let points  = polygon 31 255 (Vect 470 470 0 1)
        edges   = concat [[x,y] | x <- points, y <- points]
        drawing = (drawEdges red edges) $ mempty
    writeFile "out.ppm" (printPixels (900, 900) drawing)

drawEdges :: Color -> [Vect Int] -> Screen -> Screen
drawEdges c edges = mconcat . map (drawLine c . uncurry Line) $ (pairOff edges)

mmult :: (Num a, Functor f) => Transform a -> f (Vect a) -> f (Vect a)
mmult t = fmap (pmult t)

pmult :: (Num a) => Transform a -> Vect a -> Vect a
pmult t = liftA2 dot t . pure

dot :: (Num a) => Vect a -> Vect a -> a
dot p = sum . liftA2 (*) p

drawLine :: Color -> Line -> Screen -> Screen
drawLine c ln = mconcat (map (plotPt c) (rasterLine ln))

plotPt :: Color -> Vect Int -> Screen -> Screen
plotPt c (Vect x y z _) = M.insert (x, y) c

addLine :: Line -> [Vect Int] -> [Vect Int]
addLine (Line p0 p1) = ([p0, p1] ++)

-- takes bounds and a screen and puts in ppm format
printPixels :: (Int, Int) -> Screen -> String
printPixels (w, h) pxs =
    ppmHeader (w, h)
    ++ (unlines . map unwords $ [[show . f $ M.lookup (x, y) pxs
                | x <- [0..w-1]] | y <- [0..h-1]])
    where   f Nothing  = Color 0 0 0
            f (Just c) = c 
 
ppmHeader :: (Int, Int) -> String
ppmHeader (w, h) = "P3 " ++ show w ++ " " ++ show h ++ " 255\n"

-- all (non-permuted) pairs of a list
allPairs :: [a] -> [(a, a)]
allPairs []     = []
allPairs (_:[]) = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs
-- DIFFERENT: pair off elements of a list
pairOff :: [a] -> [(a, a)]
pairOff []       = []
pairOff (x:[])   = []
pairOff (a:b:xs) = ((a,b) : pairOff xs)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- floating point math :'(
polygon :: (Integral a) => a -> a -> Vect a -> [Vect a]
polygon s r (Vect x y z q) =
    [Vect  (x + round (fromIntegral r * sin ((fromIntegral i) * th)))
            (y + round (fromIntegral r * cos ((fromIntegral i) * th)))
            0 1
        | i <- [0..s-1]]
    where th = (2 * _pi) / (fromIntegral s)
-- cmon haskell I'm just trying to multiply a sine why do you have to make
--  the typing so hard

-- just gives you the points a line covers, no color
rasterLine :: Line -> [Vect Int]
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
_rLx :: Line -> [Vect Int]
_rLx (Line (Vect x0 y0 _ _) (Vect x1 y1 _ _)) =
    L.zipWith4 (Vect) [x0..x1] ys (repeat 0) (repeat 1)
    where   ys = map ((+y0) . (`quot` (2* abs dx))) . tail $ [negate dy, dy..]
            dy = y1 - y0
            dx = x1 - x0

_rLy :: Line -> [Vect Int]
_rLy (Line (Vect x0 y0 _ _) (Vect x1 y1 _ _)) =
    L.zipWith4 (Vect) xs [y0..y1] (repeat 0) (repeat 1)
    where   xs = map ((+x0) . (`quot` (2* abs dy))) . tail $ [negate dx, dx..]
            dy = y1 - y0
            dx = x1 - x0
