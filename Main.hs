module Main where

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import Cards    -- Fuda
import Hands    -- Tefuda

type DiscardList = [Card]   -- Sutefuda
type Deck = [Card]          -- Yamafuda


-- | Draw cards to make new hand from Deck
-- Return a new Hand and Deck if its possible.
--
-- >>> 
drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
drawHand deck dis h = let 
    nl = filter (`notElem` dis) (fromHand h)
    nr = drop (5 - length nl) deck
    in (,) <$> toHand (take 5 $ nl ++ deck) <*> Just nr

    -- in do
    --     hand <- toHand . take 5 $ nl ++ deck
    --     return (hand, nr)

-- | Get hand from deck(Yamafuda)
--
-- >>> let Just (hand, newDeck) = getHand allCards
-- >>> hand
-- Hand {fromHand = [H2_,H3_,H4_,H5_,H6_]}
-- 
-- >>> let Just (_, newDeck') = getHand newDeck
-- >>> take 8 newDeck'
-- [HQ_,HK_,HA_,D2_,D3_,D4_,D5_,D6_]
--
-- >>> getHand allCards >>= return . snd >>= getHand >>= return . take 8 . snd
-- Just [HQ_,HK_,HA_,D2_,D3_,D4_,D5_,D6_]
getHand :: Deck -> Maybe (Hand, Deck)
getHand deck = do
    hand <- toHand . take 5 $ deck
    return (hand, drop 5 deck)

-- | Get discardList(Sutefuda) from hand
getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do
    input <- getLine
    return . Just . selectByIndexes (fromHand h) $ toIntList input

-- | String to [Int] for parse user inputs
-- 
-- >>> toIntList "1234"
-- [1,2,3,4]
--
-- >>> toIntList "z4q01"
-- [4,1]
--
-- >>> toIntList ""
-- []
toIntList :: String -> [Int]
toIntList = map digitToInt . filter ((`isInfixOf` "12345") . (:[]))

-- | Get cards by indexes
--
-- >>> selectByIndexes "12345" [1..3]
-- "123"
--
-- todo: > selectByIndexes "12345" [10]
-- "*** Exception: Prelude.(!!): index too large
--
selectByIndexes :: [a] -> [Int] -> [a]
selectByIndexes xs = map ((xs!!) . subtract 1)


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
