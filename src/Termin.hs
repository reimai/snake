--module for supid & simple console games
module Termin (
    addToScreen, readAll, startGame, normalize, GameState(..) 
) where

import Data.List
import System.Timeout
import System.Console.ANSI
import qualified System.Console.Terminal.Size as T
import System.Console.Terminal.Size (Window)
import Data.Maybe
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Control.Monad
import Control.Concurrent
import System.IO
import Control.Concurrent
import System.CPUTime
import Geom
import System.Random

data GameState = GameState{wnd :: (Int, Int), rnd :: StdGen}


startGame :: Renderable a => (a -> Char -> GameState -> (a, GameState)) -> (GameState -> (a, GameState)) -> IO()
startGame action initWorld = do
        hSetBuffering stdin NoBuffering --get input immedietly
        hSetEcho stdin False            --don't show the typed character
        jwindow <- T.size
        let wnd = fromJust jwindow
        rnd <- getStdGen
        let w = T.width wnd
        let h = T.height wnd - 1
        let (initRnd, mainRnd) = split rnd
        let game = GameState (w, h) mainRnd
        let world = fst $ initWorld $ GameState (w, h) initRnd
        gameLoop stdin game world action


--main loop, read input, change the world, redraw screen 
gameLoop :: Renderable a => Handle -> GameState -> a -> (a -> Char -> GameState -> (a, GameState)) -> IO()
gameLoop input game@(GameState (w, h) rnd) world action = do
                        mapM_ putStrLn $ addToScreen (replicate h $ replicate w ' ') (w,h) world 
                        e <- threadDelay (floor(1/fps * 10^6))  
                        ch <- readAll input ' '
                        let (newWorld, newGame) = action world ch game
                        when (ch /= 'q') $ gameLoop input newGame newWorld action
                            where fps = 15


addToScreen :: Renderable a => [String] -> (Int, Int) -> a -> [String]
addToScreen ls wnd obj = map (\(i, l, ps) -> mergeLine l ps) $ groupByLines ls $ filter (\p -> fits wnd $ crd p) $ render obj  

fits :: (Int,Int) -> Crd -> Bool
fits (w,h) (Crd x y) = x < w && x >= 0 && y >= 0 && y < h                             

normalize :: (Int, Int) -> Crd -> Crd
normalize (w, h) (Crd x y) = Crd (x `mod` w) (y `mod` h) 

mergeLine :: String -> [Point] -> String
mergeLine line ps = mergeLine' (zip [0..] line) $ sortOn (x.crd) ps 

mergeLine' :: [(Int, Char)] -> [Point] -> String
mergeLine' l [] = map snd l 
mergeLine' [] p = error $ "no cols " ++ (show p)
mergeLine' lines@((il, l):ls) (p:ps) | il <  (x.crd $ p) = l:(mergeLine' ls (p:ps))
                               | il == (x.crd $ p) = (sym p):(mergeLine' ls ps)
                               | otherwise         = mergeLine' lines ps --duplicate crd


groupByLines  :: [String] -> [Point] -> [(Int, String, [Point])]
groupByLines ls ps = mergeToLines (zip3 [0..] ls $ repeat []) $ map (\p -> (y.crd $ p, p)) $ sortOn (y.crd) ps

mergeToLines :: (Ord a, Show a, Show c) => [(a, b, [c])] -> [(a, c)] -> [(a, b, [c])]
mergeToLines x [] = x
mergeToLines [] y = error $ "no lines" ++ (show y)
mergeToLines ((x, xd, xyd):xs) ((y, yd):ys) | x < y     = (x, xd, xyd):(mergeToLines xs ((y, yd):ys))
                                            | otherwise = mergeToLines ((x, xd, yd:xyd):xs) ys 
               
--read all input from Handle, return only the last char or default
readAll :: Handle -> Char -> IO(Char)
readAll h defaultCh = hReady h >>= \gotIt -> if gotIt then (hGetChar h >>= readAll h) else return defaultCh
