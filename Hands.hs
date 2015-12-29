module Hands
    ( Hand
    , toHand
    , fromHand
    ) where


import Data.List
import Data.Function
import Control.Applicative
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
-- >>> let treeCards = take 3 $ filter ((2 ==) . cardNumber) $ allCards
-- >>> let twoCards = take 2 $ filter ((10 ==) . cardNumber) $ allCards
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



