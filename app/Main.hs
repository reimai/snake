{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Termin
import Geom
import Data.Maybe

main :: IO()
main = startGame action newWorld 

newWorld :: World
newWorld = World (newSnake (Crd 6 4) 14) False []

newSnake :: Crd -> Int -> Snake
newSnake (Crd x y) len = Snake $ reverse [Crd xi y | xi <- [x..x+len] ]

emptyTitle :: Title
emptyTitle = Title "" $ Crd 0 0

action :: World -> Char -> (Int,Int) -> World
action world@(World (Snake s) dead rndr) ch (w, h) | dead = world
                                                   | otherwise = if (isDead nextSnake) then (World nextSnake True [youDied (w, h)]) else World nextSnake False []
    where nextSnake = Snake $ move (head s) dir 1 : init s  
          dir = fromMaybe prevDir $ fmap (\d -> if (isOpposite d prevDir) then prevDir else d) (getDir ch)
          prevDir = getPrevDir s         

getDir :: Char -> Maybe Dir
getDir 'D' = Just LeftD
getDir 'A' = Just UpD
getDir 'C' = Just RightD
getDir 'B' = Just DownD
getDir _   = Nothing

getPrevDir :: [Crd] -> Dir
getPrevDir s@((Crd ax ay):(Crd bx by):ts) | ax < bx = LeftD
                                          | ax > bx = RightD
                                          | ay < by = UpD
                                          | ay > by = DownD
                                          | otherwise = error $ "s: "++ show s

isDead :: Snake -> Bool
isDead (Snake (s:ss)) = s `elem` ss

youDied :: (Int, Int) -> Title
youDied = center "YOU DIED" 

data Snake = Snake {body :: [Crd]}
    deriving (Eq, Show)

instance Renderable Snake where
    render (Snake s_body) = Point (head s_body) '▒' : map (\c -> Point c '█') (init.tail $ s_body) ++ [Point (last s_body) '█']

data World = World {snake :: Snake, dead :: Bool, msg :: [Title]}
instance Renderable World where
    render (World snake dead msg) = concat $ map render (RS snake:(map RS msg)) --great type system, they said 

data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a
