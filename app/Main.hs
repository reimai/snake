{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Termin
import Geom
import Data.Maybe
import Data.List
import Control.Monad.Trans.State
import System.Random

main :: IO()
main = startGame action newWorld 

newWorld :: GameAux -> Game World
newWorld aux = Game newAux $ World (newSnake (Crd 6 4) 6) [] 0 treats
    where (treats, newAux) = runState getTreats aux

newSnake :: Crd -> Int -> Snake
newSnake (Crd x y) len = Snake (reverse [Crd xi y | xi <- [x..x+len]]) False RightD

action :: Game World -> Char -> Game World
action game@(Game aux world@(World snake@(Snake s dead prevDir) msg score treats)) ch | dead = game
                                                                                           | otherwise = Game newAux newWorld
    where newWorld = World nextSnake messages newScore newTreats
          messages = [debug snake, showScore newScore] ++ (if (isDead nextSnake) then [youDied (wnd aux)] else [])
          newScore = score + gotScore
          (nextSnake, newTreats, gotScore, newAux) = moveSnake snake aux (getDir ch) treats

moveSnake :: Snake -> GameAux -> Maybe Dir -> [Treat] -> (Snake, [Treat], Int, GameAux)
moveSnake (Snake body@(s:ss) dead prevDir) aux@(GameAux wnd rnd) maybeDir ts = (checkDead $ newSnake, newTreats, gotScore, newAux)
    where newSnake = Snake (newHead : (if (isJust ate) then body else init body)) dead dir
          dir = fromMaybe prevDir $ fmap (\d -> if (isOpposite d prevDir) then prevDir else d) maybeDir
          gotScore = fromMaybe 0 $ fmap (\i -> life $ ts !! i) ate
          ate = elemIndex newHead (map (crd.p) ts)
          newHead = normalize wnd $ move s dir 1
          (extraTreats, newAux) = runState getTreats aux
          newTreats = extraTreats ++ (catMaybes $ map getOlder $ fromMaybe ts $ fmap (\i -> deleteByIndex i ts) ate)

getOlder :: Treat -> Maybe Treat
getOlder (Treat p life) | life == 1 = Nothing
                        | otherwise = Just $ Treat p $ life-1

deleteByIndex :: Int -> [a] -> [a]
deleteByIndex i xs = fst splitted ++ (tail $ snd splitted) 
    where splitted = splitAt i xs

showScore :: Int -> Title
showScore score = Title ("Score: " ++ (show score)) $ Crd 0 0

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
instance Renderable Snake where
    render (Snake sBody dead _) = Point (head sBody) headSym : map (\c -> Point c '█') (init.tail $ sBody) ++ [Point (last sBody) '█']
        where headSym = if dead then 'X' else '▒'

treats_life :: (Int, Int) -> Int
treats_life (w, h) = w `div` 2 + h `div` 2 + 3

data Treat = Treat {p :: Point, life :: Int}
instance Renderable Treat where
    render (Treat p life) = render p

data World = World {snake :: Snake, msg :: [Title], score :: Int, treats :: [Treat]}
instance Renderable World where
    render (World snake msg score treats) = concat $ map render (RS snake:(map RS treats) ++ (map RS msg)) --great type system, they said

data Renderables = forall a. Renderable a => RS a
instance Renderable Renderables where
    render (RS a) = render a

getLife :: AuxState Int
getLife  = state (\(GameAux wnd rnd) -> (treats_life wnd, GameAux wnd rnd))

getTreat :: AuxState Treat
getTreat = getCrd >>= \crd -> getLife >>= \life -> return $ Treat (Point crd '*') life

--toss a coin with a chance to win somehow proportional to window's size
headsOrTails :: AuxState Bool
headsOrTails = state (\(GameAux wnd rnd) -> (\(i, rnd') -> (i==1, GameAux wnd rnd')) $ randomR (0, treats_life wnd `div` 2) rnd)

getTreats :: AuxState [Treat]
getTreats = headsOrTails >>= \win -> if win then sequence [getTreat] else return []