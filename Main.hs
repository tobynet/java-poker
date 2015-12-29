module Main where

import System.Random.Shuffle
import Control.Monad
import Cards
import Hands

randomHand :: IO (Maybe Hand)
randomHand = do 
    shuffled <- shuffleM allCards
    return . toHand . take 5 $ shuffled


judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker = liftM pokerHand

main :: IO ()
main = forM_ [1..500] $ \x -> do
    hand <- randomHand
    let res = judgePoker hand
    putStrLn $ show (x::Integer) ++ "  " ++ show hand ++ " -> " ++ show res
