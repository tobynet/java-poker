module Game.Poker
    ( module Game.Poker.Hands
    , module Game.Poker.Cards
    , simpleGame
    ) where

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe
import Game.Poker.Cards    -- Fuda
import Game.Poker.Hands    -- Tefuda

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


simpleGame :: IO ()
simpleGame = do
    -- Ogre font
    putStrLn "     __                     ___      _             "
    putStrLn "     \\ \\  __ ___   ____ _  / _ \\___ | | _____ _ __ "
    putStrLn "      \\ \\/ _` \\ \\ / / _` |/ /_)/ _ \\| |/ / _ \\ '__|"
    putStrLn "   /\\_/ / (_| |\\ V / (_| / ___/ (_) |   <  __/ |   "
    putStrLn "   \\___/ \\__,_| \\_/ \\__,_\\/    \\___/|_|\\_\\___|_|   "
    putStrLn "                                                   "

    deck <- shuffleM allCards
    case getHand deck of
        Nothing -> error "Unexpected error"
        Just res -> matchPoker res
    ynQuestion "-- replay?" simpleGame (putStrLn "-- bye.")


data Player = Player | Enemy deriving Eq

showPlayerName :: Player -> String
showPlayerName Player = "You"
showPlayerName Enemy = "Java"

matchPoker :: (Hand, Deck) -> IO ()
matchPoker (mhand, deck) = do
    (mres, ndeck, nmhand) <- playPoker mhand deck Player
    case getHand ndeck of
        Nothing -> error "Unexpected error"
        Just (ehand, odeck) -> do
            (eres, _,nehand) <- playPoker ehand odeck Enemy
            printResult nmhand nehand mres eres


-- | Play poker
playPoker :: Hand -> Deck -> Player -> IO ((PokerHand, Card), Deck, Hand)
playPoker hand deck player = do
    discards <- if player == Player
        then inputDisuse hand
        else aiDisuse hand

    case drawHand deck discards hand of
        Nothing -> error "Unexpected error"
        Just (nhand, ndeck) -> do
            let res = pokerHand nhand
            return (res, ndeck, nhand)


-- | Input suteru cards
inputDisuse :: Hand -> IO DiscardList
inputDisuse hand = do
    printHand [] hand Player
    putStrLn "-- Select discardable cards" 
    gotDisuse <- getDiscardList hand
    case gotDisuse of
        Nothing -> do
            putStrLn "-- Input 1 or .. 5" 
            inputDisuse hand
        Just disuses -> do
            printHand disuses hand Player
            ynQuestion "-- You: OK?" (return disuses) (inputDisuse hand)


-- | aiDisuse :: Hand -> IO DiscardList
aiDisuse :: Hand -> IO DiscardList
aiDisuse hand = do
    let res = aiSelectDiscards hand
    printHand res hand Enemy
    putStrLn "-- Java: OK!"
    return res


-- | print Yaku
printResult :: Hand -> Hand -> (PokerHand, Card) -> (PokerHand, Card) -> IO()
printResult mhand ehand mres@(mph, mcard) eres@(eph, ecard) = do
    putStrLn "***** Result *****"
    printHand [] mhand Player
    printHand [] ehand Enemy

    putStrLn $ concat ["Your hand is ", show mph, ", greatest card is ", show mcard]
    putStrLn $ concat ["Java's hand is ", show eph, ", greatest card is ", show ecard]

    putStrLn $ concat 
        [ "\n"
        , "      +----------------------------+\n"
        , "      ||                          ||\n"
        , "      ||        " , winLossMessage, "       ||\n"
        , "      ||                          ||\n"
        , "      +----------------------------+\n" 
        ]
    where 
        winLossMessage = case judgeVictory mres eres of 
            LT -> " Java win! "
            EQ -> "It's a tie!"
            GT -> " You win!! "


-- | print Tefuda
printHand :: DiscardList -> Hand -> Player -> IO ()
printHand dis hand player = do
    putStrLn $ "-- " ++ showPlayerName player ++ "'s Hand : "
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


-- | Sutefuda = Hand - allYaku
--
-- >>> let Just (x) = toHand $ take 5 $ (filter (\x -> (cardNumber x == 10)) $ allCards) ++ allCards
-- >>> fromHand x
-- [H2_,H10,D10,C10,S10]
-- >>> nOfKindDiscards x 
-- [H2_]
nOfKindDiscards :: Hand -> DiscardList
nOfKindDiscards hand = fromHand hand \\ allNOfKinds hand
    where
        -- | all Yaku
        --
        -- >>> let Just (x) = toHand $ take 5 $ (filter (\x -> (cardNumber x == 10)) $ allCards) ++ allCards
        -- >>> allNOfKinds x
        -- [H10,D10,C10,S10]
        allNOfKinds :: Hand -> [Card]
        allNOfKinds h = concat . concat $ catMaybes [nOfKindHint 2 h, nOfKindHint 3 h, nOfKindHint 4 h]


-- | Sutefuda by AI
--
-- >>> let Just straightFlush = toHand $ take 5 $ allCards
-- >>> aiSelectDiscards straightFlush
-- []
--
-- >>> let Just fourCard = toHand $ take 5 $ (filter ((==10) . cardNumber) allCards) ++ allCards
-- >>> aiSelectDiscards fourCard
-- [H2_]
--
-- >>> let Just buta = toHand $ take 5 $ (take 2 allCards) ++ (take 2 $ drop (13+5) allCards) ++ (drop (13*2+9) allCards)
-- >>> aiSelectDiscards buta
-- [H2_,H3_,D7_,D8_,CJ_]
--
aiSelectDiscards :: Hand -> DiscardList
aiSelectDiscards hand =
    fromMaybe (nOfKindDiscards hand)
        ((straightHint hand <|> flushHint hand) *> Just [])


-- | Judge victory you and AI
--
-- >>>
judgeVictory :: (PokerHand, Card) -> (PokerHand, Card) -> Ordering
judgeVictory l r = compare (pullStrength l) (pullStrength r)
    where
        pullStrength :: (PokerHand, Card) -> (PokerHand, Int)
        pullStrength = fmap cardStrength


