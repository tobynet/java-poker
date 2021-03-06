module Game.Poker.Hands
    ( Hand
    , toHand, fromHand
    , pokerHand
    , PokerHand
    
    -- hint
    , straightHint
    , flushHint
    , nOfKindHint

    -- hand
    , fourOfAKind
    , fullHouse
    , flush
    , straight
    , threeOfAKind
    , twoPair
    , onePair

    , DiscardList
    , Deck
    , drawHand
    , getHand
    , getDiscardList
    , toIntList
    , selectByIndexes
    , straightFlush
    , judgeVictory
    ) where


import Data.List
import Data.Char
import Data.Function
import Data.Maybe
import Control.Applicative
import Control.Monad
import Game.Poker.Cards

-- | Constrained cards in hand
--
-- >>> :type fromHand
-- fromHand :: Hand -> [Card]
--
newtype Hand = Hand { fromHand :: [Card] }
    deriving (Show, Eq, Ord)

-- | Cards to Hard
--
-- >>> toHand allCards
-- Nothing
--
-- >>> fmap (length . fromHand) (toHand $ take 5 allCards)
-- Just 5
--
toHand :: [Card] -> Maybe Hand
toHand xs = 
    if length xs == 5
    then Just $ Hand (sort xs)
    else Nothing


-- 
data PokerHand
    = HighCards         -- Buta
    | OnePair           --  ^
    | TwoPair           --  |
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse         --  |
    | FourOfAKind       --  V
    | StraightFlush     -- Sugoi
    deriving (Show, Read, Eq, Ord, Enum)

type DiscardList = [Card]   -- Sutefuda
type Deck = [Card]          -- Yamafuda

-- | Detect poker hand and return strength Card
-- 
-- >>> let sameNum = filter ((==14) . cardNumber) allCards
-- >>> let sameSuit = filter ((==Hearts) . cardSuit) allCards
--
-- >>> pokerHand (Hand $ take 5 sameSuit)
-- (StraightFlush,H6_)
--
-- >>> let buta = take 2 allCards ++ (take 2 $ drop 17 allCards) ++ [last allCards]
-- >>> pokerHand (Hand buta)
-- (HighCards,SA_)
--
pokerHand :: Hand -> (PokerHand, Card)
pokerHand h@(Hand xs) = 
    fromMaybe (HighCards, last xs)
        (foldl mplus Nothing $ fmap ($h) hands)
    
    where
        hands :: [Hand -> Maybe (PokerHand, Card)]
        hands = 
            [ straightFlush
            , fourOfAKind
            , fullHouse
            , flush
            , straight
            , threeOfAKind
            , twoPair
            , onePair
            ]


-- Implement every Hand!!!!

-- | Detect onePair and return strongest Card
--
-- >>> let sameNum = filter ((==9) . cardNumber) allCards
-- >>> let sameSuit = filter ((==Spades) . cardSuit) allCards
-- >>> onePair $ Hand (take 2 sameNum ++ take 3 sameSuit)
-- Just (OnePair,D9_)
-- 
-- >>> onePair $ Hand (take 5 sameSuit)
-- Nothing
onePair :: Hand -> Maybe (PokerHand, Card)
onePair  x = do
    cs <- nOfKindHint 2 x
    return (OnePair, last . concat $ cs)

    -- same as
    -- fmap (((,) OnePair) . last . join) . nOfKindHint 2

-- | Detect TwoPair and return strongest Card
--
-- >>> let sameNum = filter ((==9) . cardNumber) allCards
-- >>> let sameNum' = filter ((==10) . cardNumber) allCards
-- >>> let sameSuit = filter ((==Spades) . cardSuit) allCards
-- >>> twoPair $ Hand (take 2 sameNum ++ take 2 sameNum' ++ take 1 sameSuit)
-- Just (TwoPair,D10)
--
-- >>> twoPair $ Hand (take 2 sameNum ++ take 3 sameSuit)
-- Nothing
-- 
-- >>> twoPair $ Hand (take 5 sameSuit)
-- Nothing
twoPair :: Hand -> Maybe (PokerHand, Card)
twoPair x = do
    cs <- nOfKindHint 2 x
    guard (length cs == 2)
    return (TwoPair, last . concat $ cs)

-- | Detect ThreeOfAKind and return strongest Card
--
-- >>> let sameNum = filter ((==4) . cardNumber) allCards
-- >>> let sameSuit = filter ((==Spades) . cardSuit) allCards
-- >>> threeOfAKind $ Hand (take 3 sameNum ++ take 2 sameSuit)
-- Just (ThreeOfAKind,C4_)
-- 
-- >>> threeOfAKind $ Hand (take 5 sameSuit)
-- Nothing
threeOfAKind :: Hand -> Maybe (PokerHand, Card)
threeOfAKind  x = do
    cs <- nOfKindHint 3 x
    return (ThreeOfAKind, maximum . concat $ cs)


-- | Detect Straight and return strongest Card
--
-- >>> straight $ Hand (take 5 $ filter ((==Hearts) . cardSuit) allCards)
-- Just (Straight,H6_)
--
-- >>> straight $ Hand (take 5 $ filter (even . cardNumber) allCards)
-- Nothing
straight :: Hand -> Maybe (PokerHand, Card)
straight x = do
    c <- straightHint x
    return (Straight, c)
    
    -- Same as followings
    -- straightHint x >>= (\y -> return (Straight, y))
    -- fmap (\y -> (Straight, y)) (straightHint x)


-- | Detect Flush and return strongest Card
--
-- >>> flush $ Hand (take 5 $ filter ((==Hearts) . cardSuit ) allCards)
-- Just (Flush,H6_)
--
-- >>> flush $ Hand (take 5 $ filter ((<= 3) . cardNumber) allCards)
-- Nothing
flush :: Hand -> Maybe (PokerHand, Card)
flush x = do
    c <- flushHint x
    return (Flush, c)

    -- Same as followings
    -- flushHint x >>= (\y -> return (Straight, y))
    -- fmap (\y -> (Flush, y)) (flushHint x)


-- | Detect fullHouse and return strongest Card
--
-- >>> let sameNum = filter ((==9) . cardNumber) allCards
-- >>> let sameNum' = filter ((==10) . cardNumber) allCards
-- >>> let sameSuit = filter ((==Spades) . cardSuit) allCards
-- >>> fullHouse $ Hand (take 2 sameNum ++ take 3 sameNum')
-- Just (FullHouse,C10)
--
-- >>> fullHouse $ Hand (take 3 sameNum ++ take 2 sameNum')
-- Just (FullHouse,C9_)
--
-- >>> fullHouse $ Hand (take 2 sameNum ++ take 3 sameSuit)
-- Nothing
-- 
-- >>> fullHouse $ Hand (take 5 sameSuit)
-- Nothing
fullHouse :: Hand -> Maybe (PokerHand, Card)
fullHouse x = do
    cs <- nOfKindHint 3 x
    ds <- nOfKindHint 2 x
    guard (length cs == 1 && length ds == 1)
    return (FullHouse, last $ concat cs )

-- | Detect FourOfAKind and return strongest Card
--
-- >>> let sameNum = filter ((==4) . cardNumber) allCards
-- >>> let sameSuit = filter ((==Spades) . cardSuit) allCards
-- >>> fourOfAKind $ Hand (take 4 sameNum ++ take 1 sameSuit)
-- Just (FourOfAKind,S4_)
-- 
-- >>> fourOfAKind $ Hand (take 5 sameSuit)
-- Nothing
fourOfAKind :: Hand -> Maybe (PokerHand, Card)
fourOfAKind x = do
    cs <- nOfKindHint 4 x
    return (FourOfAKind, maximum . concat $ cs)


-- | Detect StraightFlush and return strongest Card
--
-- >>> straightFlush $ Hand (take 5 $ filter ((==Hearts) . cardSuit) allCards)
-- Just (StraightFlush,H6_)
--
-- >>> straightFlush $ Hand (take 5 $ filter (\x -> cardSuit x == Hearts && even (cardNumber x)) allCards)
-- Nothing
--
-- >>> let sameSuit = filter ((==Hearts) . cardSuit) allCards
-- >>> let sameSuit' = filter ((==Spades) . cardSuit) allCards
-- >>> straightFlush $ Hand (take 3 sameSuit ++ take 2 (drop 3 sameSuit'))
-- Nothing
--
-- >>> straightFlush $ Hand (take 5 $ filter (even . cardNumber) allCards)
-- Nothing
straightFlush :: Hand -> Maybe (PokerHand, Card)
straightFlush x = do
    c <- flushHint x
    d <- straightHint x
    return (StraightFlush, max c d)


-- | Check straight in Hand
--
-- >>> straightHint $ Hand (take 5 allCards)
-- Just H6_
--
-- >>> straightHint $ Hand (take 5 $ drop 8 allCards)
-- Just HA_
--
-- >>> straightHint $ Hand (take 2 $ allCards)
-- Nothing
straightHint :: Hand -> Maybe Card
straightHint (Hand xs) = 
    (judgeStraight . extract cardStrength $ xs)
    <|> (judgeStraight . sort . extract cardNumber $ xs)
    where
        -- | Check Straight with Numbers
        --
        -- >>> isStraight [1..5]
        -- True
        --
        -- >>> isStraight [1,3,4,5,6]
        -- False
        --
        -- >>> isStraight [1]
        -- False
        --
        -- >>> isStraight []
        -- False
        isStraight :: [Int] -> Bool
        isStraight [] = False
        isStraight ys@(y:_) = ys == [y..y+4]

        -- | Check Straight and return strongest card
        --
        -- >>> judgeStraight . extract cardNumber . sort . take 5 $ allCards
        -- Just H6_
        --
        -- >>> judgeStraight []
        -- Nothing
        judgeStraight :: [(Int, Card)] -> Maybe Card
        judgeStraight ys = 
            if isStraight $ map fst ys
            then Just . snd . last $ ys
            else Nothing


-- | Check flush in Hand
--
-- >>> flushHint $ Hand (take 5 $ filter (\x -> cardSuit x == Hearts) allCards )
-- Just H6_
--
-- >>> flushHint $ Hand (take 5 $ filter (\x -> cardNumber x == 2) allCards )
-- Nothing
flushHint :: Hand -> Maybe Card
flushHint (Hand (x:xs)) =
    if all (== suit) suits
    then Just (last xs)
    else Nothing
    where
        suit = cardSuit x
        suits = map cardSuit xs

flushHint (Hand []) = Nothing


-- | n of Kind in Hand
--
-- >>> let treeCards = take 3 $ filter ((==2) . cardNumber) $ allCards
-- >>> let twoCards = take 2 $ filter ((==10) . cardNumber) $ allCards
-- >>> let fullhouse = toHand $ treeCards ++ twoCards
--
-- >>> fullhouse >>= nOfKindHint 2
-- Just [[H10,D10]]
--
-- >>> fullhouse >>= nOfKindHint 3
-- Just [[H2_,D2_,C2_]]
--
-- >>> fullhouse >>= nOfKindHint 4
-- Nothing
--
nOfKindHint :: Int -> Hand -> Maybe [[Card]]
nOfKindHint n (Hand xs) = 
    if cards /= [] then Just cards else Nothing
    where
        cards :: [[Card]]
        cards = filter ((==n) . length) $
            groupBy ((==) `on` cardNumber) xs
        -- cards = groupBy (\x y -> cardNumber x == cardNumber y) xs


-- | 
--
-- >>> extract cardNumber $ take 5 $ allCards
-- [(2,H2_),(3,H3_),(4,H4_),(5,H5_),(6,H6_)]
--
-- >>> extract cardStrength $ take 5 $ allCards
-- [(2,H2_),(3,H3_),(4,H4_),(5,H5_),(6,H6_)]
extract :: (a -> b) -> [a] -> [(b, a)]
extract f cs = [ (f c, c) | c <- cs ]


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


-- | Judge victory you and AI
--
-- >>>
judgeVictory :: (PokerHand, Card) -> (PokerHand, Card) -> Ordering
judgeVictory l r = compare (pullStrength l) (pullStrength r)
    where
        pullStrength :: (PokerHand, Card) -> (PokerHand, Int)
        pullStrength = fmap cardStrength



