module Termin (
    Crd (..), Point (..), Renderable, render, 
    addToScreen, startGame, readAll 
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


data Crd = Crd {x :: Int, y :: Int} deriving (Eq, Show)
data Point = Point {crd :: Crd, sym :: Char} deriving (Eq, Show)

class Renderable a where 
    render :: a -> [Point]

addToScreen :: Renderable a => [String] -> (Int, Int) -> a -> [String]
addToScreen ls size obj = map (\(i, l, ps) -> mergeLine l ps) $ groupByLines ls $ normalize (render obj) size 

normalize :: [Point] -> (Int, Int) -> [Point]
normalize ps (w, h) = map (\(Point (Crd x y) s) -> Point (Crd (x `mod` w) (y `mod` h)) s) ps 

mergeLine :: String -> [Point] -> String
mergeLine line ps = mergeLine' (zip [0..] line) $ sortOn (x.crd) ps 

mergeLine' :: [(Int, Char)] -> [Point] -> String
mergeLine' l [] = map snd l 
mergeLine' [] p = error ""
mergeLine' (l:ls) (p:ps) | (fst l) < (x.crd $ p) = (snd l):(mergeLine' ls (p:ps))
                         | otherwise             = (sym p):(mergeLine' ls ps)


groupByLines  :: [String] -> [Point] -> [(Int, String, [Point])]
groupByLines ls ps = mergeToLines (zip3 [0..] ls $ repeat []) $ map (\p -> (y.crd $ p, p)) $ sortOn (y.crd) ps

mergeToLines :: Ord a => [(a, b, [c])] -> [(a, c)] -> [(a, b, [c])]
mergeToLines x [] = x
mergeToLines [] y = error "no lines"
mergeToLines ((x, xd, xyd):xs) ((y, yd):ys) | x < y     = (x, xd, xyd):(mergeToLines xs ((y, yd):ys))
                                            | otherwise = mergeToLines ((x, xd, yd:xyd):xs) ys 
               
startGame :: Renderable a => (a -> Char -> (Int, Int) -> a) -> a -> IO()                      
startGame action world = do
        hSetBuffering stdin NoBuffering --get input immedietly
        hSetEcho stdin False            --don't show the typed character
        jwindow <- T.size
        let wnd = fromJust jwindow
        gameLoop stdin wnd world action

--main loop, read input, change the world, redraw screen 
gameLoop :: Renderable a => Handle -> Window Int -> a -> (a -> Char -> (Int, Int) -> a) -> IO()
gameLoop h wnd world action = do 
                        --clearScreen
                        mapM_ putStrLn $ addToScreen (replicate height $ replicate width ' ') (width, height) world 
                        e <- threadDelay (floor(1/fps * 10^6))  
                        ch <- readAll h ' '
                        when (ch /= 'q') $ gameLoop h wnd (action world ch (width, height)) action 
                                where fps      = 15
                                      height   = T.height wnd - 1
                                      width    = T.width wnd

--read all input from Handle, return only the last char or default
readAll :: Handle -> Char -> IO(Char)
readAll h defaultCh = hReady h >>= \gotIt -> if gotIt then (hGetChar h >>= readAll h) else return defaultCh

