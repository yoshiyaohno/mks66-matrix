import qualified Line as Line

data Transform = Transform Point Point Point Point

-- Look I could make this nice and general but I'm literally only got four
--  dimensions these are not generalized matricies
ident :: Transform
ident = Transform
            Point 1 0 0 0
            Point 0 1 0 0
            Point 0 0 1 0
            Point 0 0 0 1

dot :: Point -> Point -> Int
dot (Point a b c d) (Point e f g h) = a*e + b*f + c*g + d*h

pmult :: Transform -> Point -> Point
pmult (Transform a b c d) p = (Point (dot a p) (dot 


mmult :: Transform -> [Point] -> [Point]

