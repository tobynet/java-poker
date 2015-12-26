module Hands
    ( Hand
    , toHand
    , fromHand
    ) where


import Data.List
import Cards

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
-- >>> fmap (length . fromHand) (toHand $ take 5 $ allCards)
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


pokerHand :: Hand -> (PokerHand, Card)
pokerHand = undefined

-- Implement every Hand!!!!

-- onePair :: Hand -> Maybe (PokerHand, Card)
-- onePair = undefined
-- 
-- twoPair :: Hand -> Maybe (PokerHand, Card)
-- twoPair = undefined
-- 
-- twoPair :: Hand -> Maybe (PokerHand, Card)
-- twoPair = undefined
-- 
-- threeOfAKind :: Hand -> Maybe (PokerHand, Card)
-- threeOfAKind = undefined
-- 
-- straight :: Hand -> Maybe (PokerHand, Card)
-- straight = undefined
-- 
-- flush :: Hand -> Maybe (PokerHand, Card)
-- flush = undefined
-- 
-- fullHouse :: Hand -> Maybe (PokerHand, Card)
-- fullHouse = undefined
-- 
-- fourOfAKind :: Hand -> Maybe (PokerHand, Card)
-- fourOfAKind = undefined
-- 
-- straightFlush :: Hand -> Maybe (PokerHand, Card)
-- straightFlush = undefined



