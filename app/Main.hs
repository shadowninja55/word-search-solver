module Main where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import System.Console.Pretty
import Data.List
import Control.Monad

type Puzzle = [String]
type Vec = (Int, Int)

add :: Vec -> Vec -> Vec
add (a, b) (c, d) = (a + c, b + d)

inBounds :: Puzzle -> Vec -> Bool
inBounds [] _ = False
inBounds puzzle@(row:_) (x, y) = and [ 0 <= x, x < width, 0 <= y, y < height ]
 where
  width = length row
  height = length puzzle

findWithDir :: Puzzle -> String -> Vec -> Vec -> Maybe [Vec]
findWithDir _ "" _ _ = Just []
findWithDir puzzle (c:cs) point@(x, y) dir
  | inBounds puzzle point && puzzle !! y !! x == c 
    = (point :) <$> findWithDir puzzle cs (add point dir) dir
  | otherwise = Nothing

findFromStart :: Puzzle -> String -> Vec -> Maybe [Vec]
findFromStart puzzle word start = asum ls
 where
  ls = findWithDir puzzle word start <$> ds
  ds = filter (/= (0, 0)) $ liftA2 (,) [-1..1] [-1..1]

findWord :: Puzzle -> String -> Maybe [Vec]
findWord puzzle "" = Nothing
findWord puzzle word@(c:_) = asum pointsOfDirs
 where
  pointsOfDirs = findFromStart puzzle word <$> startPoints
  startPoints = [(x, y) | (y, row) <- zip [0..] puzzle, 
    (x, c') <- zip [0..] row, c' == c]

renderWord :: Puzzle -> String -> [Vec] -> String
renderWord puzzle w l = unlines $ unwords <$> grid
 where
  grid = zipWith3 (zipWith3 pointColor) (repeat [0..]) (repeat <$> [0..]) puzzle
  pointColor x y c = (if S.member (x, y) pointSet then color Green 
    else style Faint) [c] :: String
  pointSet = S.fromList l

readLines :: IO [String]
readLines = do
  line <- getLine
  if line == ""
    then pure []
    else do
      rest <- readLines
      pure $ line : rest

main :: IO ()
main = do
  putStrLn "lines: \n"
  puzzle <- readLines
  putStrLn "words: \n"
  words <- readLines
  let pointsOfWords = findWord puzzle <$> words
  traverse_ (displayWord puzzle) $ zip words pointsOfWords
 where
  displayWord puzzle (word, points) = putStrLn $ word ++ "\n" 
    ++ renderWord puzzle word (fromMaybe [] points)
