module Geom (
Title(..), Dir(..), Crd(..), Point(..), 
Renderable, render,
move, center, isOpposite, isX, isY
) where


move :: Crd -> Dir -> Int -> Crd
move (Crd x y) RightD n = Crd (x+n) y 
move (Crd x y) LeftD n = Crd (x-n) y  
move (Crd x y) UpD n = Crd x (y-n)  
move (Crd x y) DownD n = Crd x (y+n)

data Crd = Crd {x :: Int, y :: Int} deriving (Eq, Show)

data Point = Point {crd :: Crd, sym :: Char} deriving (Eq, Show)
instance Renderable Point where
    render p = [p]

class Renderable a where  
    render :: a -> [Point] 

data Title = Title {str :: String, pos :: Crd}
    deriving (Eq, Show)

instance Renderable Title 
    where render (Title str (Crd x y)) = map (\(i, s) -> Point (Crd (x+i) y) s) $ zip [0..] str

data Dir = RightD | LeftD | UpD | DownD
    deriving (Eq, Show)

center :: String -> (Int, Int) -> Title
center str (w, h) = Title str $ Crd ((w - (length str)) `div` 2) (h `div` 2)

isOpposite :: Dir -> Dir -> Bool
isOpposite a b = (a /= b) && (isX a) == (isX b)

isX :: Dir -> Bool
isX LeftD = True
isX RightD = True
isX _ = False

isY :: Dir -> Bool
isY = not.isX
