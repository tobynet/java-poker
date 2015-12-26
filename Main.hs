module Main where

import System.Random.Shuffle
import Data.List
import Cards

main :: IO ()
main = do 
    shuffled <- shuffleM allCards
    print . sort . take 5 $ shuffled
