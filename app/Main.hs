{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Termin
import Geom
import Data.Maybe
import Data.List

main :: IO()
main = startGame action newWorld 

newWorld :: World
newWorld = World (newSnake (Crd 6 4) 14) False [] [meatball 10 10, meatball 20 30]
newSnake :: Crd -> Int -> Snake
newSnake (Crd x y) len = Snake $ reverse [Crd xi y | xi <- [x..x+len] ]

meatball :: Int -> Int -> Treat
meatball x y = Point (Crd x y) '*' 

emptyTitle :: Title
emptyTitle = Title "" $ Crd 0 0

action :: World -> Char -> (Int,Int) -> World
action world@(World snake@(Snake s) dead msg treats) ch (w, h) | dead = world
                                                               | otherwise = if (isDead nextSnake) then (World nextSnake True [youDied (w, h)] nextTreats) else World nextSnake False [] nextTreats
    where nextSnake = fst nextSnT
          nextTreats = snd nextSnT
          nextSnT = next snake dir treats   
          dir = fromMaybe prevDir $ fmap (\d -> if (isOpposite d prevDir) then prevDir else d) (getDir ch)
          prevDir = getPrevDir s         

next :: Snake -> Dir -> [Treat] -> (Snake, [Treat]) 
next (Snake body@(s:ss)) dir ts = (Snake $ newHead : (if (isJust ate) then body else init body), fromMaybe ts $ fmap (\i -> deleteByIndex i ts) ate)
    where ate = elemIndex newHead (map crd ts)
          newHead = move s dir 1  
          
deleteByIndex :: Int -> [a] -> [a]
deleteByIndex i xs = fst splitted ++ (tail $ snd splitted) 
    where splitted = splitAt i xs

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

type Treat = Point

instance Renderable Snake where
    render (Snake s_body) = Point (head s_body) '▒' : map (\c -> Point c '█') (init.tail $ s_body) ++ [Point (last s_body) '█']

data World = World {snake :: Snake, dead :: Bool, msg :: [Title], treats :: [Treat]}
instance Renderable World where
    render (World snake dead msg treats) = concat $ map render (RS snake:(map RS msg) ++ (map RS treats)) --great type system, they said 

data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a
