module Main where

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
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
    return $ do
        xs <- toIntList input
        selectByIndexes (fromHand h) xs

-- | String to [Int] for parse user inputs
-- 
-- >>> toIntList "1234"
-- Just [1,2,3,4]
--
-- >>> toIntList "4019"
-- Just [4,0,1,9]
--
-- >>> toIntList "z4q01"
-- Nothing
--
-- >>> toIntList ""
-- Just []
toIntList :: String -> Maybe [Int]
toIntList cs =
    if isDigits cs
    then Just $ toInts cs
    else Nothing
    where
        isDigits :: String -> Bool
        isDigits = all isDigit

        toInts :: String -> [Int]
        toInts = map digitToInt

-- | Get cards by indexes
--
-- >>> selectByIndexes "12345" [1..3]
-- Just "123"
--
-- >>> selectByIndexes "12345" [10]
-- Nothing
--
selectByIndexes :: [a] -> [Int] -> Maybe [a]
selectByIndexes xs = 
    mapM (atMay xs . subtract 1)

    where
        atMay :: [a] -> Int -> Maybe a
        atMay ys i =
            if (0 <= i) && (i < length xs)
            then Just (ys !! i)
            else Nothing

randomHand :: IO (Maybe Hand)
randomHand = do 
    shuffled <- shuffleM allCards
    return . toHand . take 5 $ shuffled


judgePoker :: Maybe Hand -> Maybe (PokerHand, Card)
judgePoker = liftM pokerHand


main :: IO ()
main = do
    putStrLn "             _       _   ___      _             "
    putStrLn "   _ __ ___ (_)_ __ (_) / _ \\___ | | _____ _ __ "
    putStrLn "  | '_ ` _ \\| | '_ \\| |/ /_)/ _ \\| |/ / _ \\ '__|"
    putStrLn "  | | | | | | | | | | / ___/ (_) |   <  __/ |   "
    putStrLn "  |_| |_| |_|_|_| |_|_\\/    \\___/|_|\\_\\___|_|   "
    putStrLn "                                                "

    deck <- shuffleM allCards
    case getHand deck of
        Nothing -> error "Unexpected error"
        Just (hand, newDeck) -> playPoker hand newDeck
    ynQuestion "-- replay?" main (putStrLn "-- bye.")


-- | Play poker
playPoker :: Hand -> Deck -> IO ()
playPoker hand deck = do
    discards <- inputDisuse hand
    case drawHand deck discards hand of
        Nothing -> error "Unexpected error"
        Just (nhand, _) -> do
            printHand [] nhand
            printResult $ pokerHand nhand


-- | Input suteru cards
inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
    printHand [] hand
    putStrLn "-- Select discardable cards" 
    gotDisuse <- getDiscardList hand
    case gotDisuse of
        Nothing -> do
            putStrLn "-- Input 1 or .. 5" 
            inputDisuse hand
        Just disuses -> do
            printHand disuses hand
            ynQuestion "-- OK?" (return disuses) (inputDisuse hand)

-- | print Yaku
printResult :: (PokerHand, Card) -> IO ()
printResult (ph, card) = putStrLn $ concat
    ["***** Your hand is ", show ph, ", greatest card is ", show card, " *****"]

-- | print Tefuda
printHand :: DiscardList -> Hand -> IO ()
printHand dis hand = do
    putStrLn "-- Hand : "
    forM_ [0..4] $ \x ->
        putStrLn $ "      " ++ showChangeHand dis hand x


-- | Repeat y/n question
ynQuestion :: String -> IO a -> IO a -> IO a
ynQuestion s yes no = do
    putStrLn $ s ++ "(y/n)"
    input <- getLine
    case input of
        "y" -> yes
        "n" -> no
        _ -> do
            putStrLn "-- Input `y` or `n`"
            ynQuestion s yes no


-- | Hand with Sutefuda to String
showChangeHand :: DiscardList -> Hand -> Int -> String
showChangeHand dis hand row = let
    judge x = if x `elem` dis 
              then
                [ "        "
                , "        "
                , "  " ++ show x ++ "   "
                , "        "
                , "        "] !! row
                
              else
                [ " -----+ "
                , "|     | "
                , "| " ++ show x ++ " | "
                , "|     | "
                , "+-----  "] !! row

    in concatMap judge (fromHand hand)


