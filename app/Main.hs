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

readLines :: IO [String]
readLines = do
  line <- getLine
  if line == ""
    then pure []
    else do
      rest <- readLines
      pure $ line : rest

inBounds :: Puzzle -> Vec -> Bool
inBounds pz p = 0 <= fst p && fst p < length (head pz) && 0 <= snd p && snd p < length pz

findWithDir :: Puzzle -> String -> Vec -> Vec -> Maybe [Vec]
findWithDir _ "" _ _ = Just []
findWithDir pz (c:cs) p d = if inBounds pz p && pz !! snd p !! fst p == c
  then (p :) <$> findWithDir pz cs (add p d) d
  else Nothing

findFromStart :: Puzzle -> String -> Vec -> Maybe [Vec]
findFromStart pz w s = asum ls
 where
  ls = findWithDir pz w s <$> ds
  ds = filter (/= (0, 0)) $ liftA2 (,) [-1..1] [-1..1]

findWord :: Puzzle -> String -> Maybe [Vec]
findWord pz w = asum ls
 where
  ls = findFromStart pz w <$> ss
  ss = [(x, y) | (y, r) <- zip [0..] pz, (x, c) <- zip [0..] r, c == head w]

renderWord :: Puzzle -> String -> [Vec] -> String
renderWord pz w l = unlines $ unwords <$> cvls
 where
  cvls = zipWith3 (zipWith3 f) (repeat [0..]) (repeat <$> [0..]) pz
  f x y c = (if S.member (x, y) s
    then color Green
    else style Faint) [c] :: String
  s = S.fromList l

main :: IO ()
main = do
  putStrLn "lines: \n"
  pz <- readLines
  putStrLn "words: \n"
  ws <- readLines
  let ls = findWord pz <$> ws
  traverse_ (f pz) $ zip ws ls
 where
  f pz (w, l) = do
    putStrLn w
    putStrLn $ renderWord pz w (fromMaybe [] l)
    void getLine
