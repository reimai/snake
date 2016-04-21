{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Termin
import Geom
import Data.Maybe
import Data.List
import qualified System.Random as R 

main :: IO()
main = startGame action newWorld 

newWorld :: GameState -> World
newWorld game = World (newSnake (Crd 6 4) 16) [] $ genTreats game 30 

genTreats :: GameState -> Int -> [Treat]
genTreats (GameState (w,h) rnd) n = take n $ map (uncurry meatball) $ zip (R.randomRs (0, w) rnd_x) (R.randomRs (0,h) rnd_y)
    where (rnd_x, rnd_y) = R.split rnd

newSnake :: Crd -> Int -> Snake
newSnake (Crd x y) len = Snake (reverse [Crd xi y | xi <- [x..x+len]]) False RightD

meatball :: Int -> Int -> Treat
meatball x y = Point (Crd x y) '*' 

action :: World -> Char -> GameState -> World
action world@(World snake@(Snake s dead prevDir) msg treats) ch game | dead = world
                                                                     | otherwise = if (isDead nextSnake) 
                                                                           then (World nextSnake [youDied (wnd game), debug snake] nextTreats) 
                                                                           else World nextSnake [debug snake] nextTreats
    where (nextSnake, nextTreats) = next snake game (getDir ch) treats   

next :: Snake -> GameState -> Maybe Dir -> [Treat] -> (Snake, [Treat]) 
next (Snake body@(s:ss) dead prevDir) (GameState wnd rnd) m_dir ts = (checkDead $ newSnake, fromMaybe ts $ fmap (\i -> deleteByIndex i ts) ate)
    where newSnake = Snake (newHead : (if (isJust ate) then body else init body)) dead dir
          dir = fromMaybe prevDir $ fmap (\d -> if (isOpposite d prevDir) then prevDir else d) m_dir 
          ate = elemIndex newHead (map crd ts)
          newHead = normalize wnd $ move s dir 1  
          
deleteByIndex :: Int -> [a] -> [a]
deleteByIndex i xs = fst splitted ++ (tail $ snd splitted) 
    where splitted = splitAt i xs

debug :: Show a => a -> Title
--debug obj = Title (show obj) $ Crd 1 1
debug obj = Title "" $ Crd 1 1

getDir :: Char -> Maybe Dir
getDir 'D' = Just LeftD
getDir 'A' = Just UpD
getDir 'C' = Just RightD
getDir 'B' = Just DownD
getDir _   = Nothing

checkDead :: Snake -> Snake
checkDead (Snake (s:ss) dead prevDir) = Snake (s:ss) (dead || died) prevDir
    where died = s `elem` ss

youDied :: (Int, Int) -> Title
youDied = center "YOU DIED" 

data Snake = Snake {body :: [Crd], isDead :: Bool, prevDir :: Dir}
    deriving (Eq, Show)

type Treat = Point

instance Renderable Snake where
    render (Snake s_body dead _) = Point (head s_body) head_sym : map (\c -> Point c '█') (init.tail $ s_body) ++ [Point (last s_body) '█']
        where head_sym = if dead then 'X' else '▒'

data World = World {snake :: Snake, msg :: [Title], treats :: [Treat]}
instance Renderable World where
    render (World snake msg treats) = concat $ map render (RS snake:(map RS treats) ++ (map RS msg)) --great type system, they said 

data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a
