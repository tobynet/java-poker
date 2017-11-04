module Game.Poker.Cards 
    ( Suit(..)
    , Card
    , allCards
    , cardSuit
    , cardNumber
    , cardStrength
    ) where

-- | 4 types of card
--
-- >>> Hearts                       -- Show
-- Hearts
-- 
-- >>> read "Hearts" :: Suit        -- Read
-- Hearts
--
-- >>> Hearts == Hearts             -- Eq
-- True
--
-- >>> Hearts == Spades             -- Eq
-- False
--
-- >>> Hearts < Diamonds            -- Ord
-- True
--
-- >>> succ Hearts                  -- Enum
-- Diamonds
--
data Suit = Hearts | Diamonds | Clubs | Spades 
    deriving (Show, Read, Eq, Ord, Enum)


-- | One playing card
--
-- >>> Card 1 Hearts == Card 2 Hearts       -- Eq
-- False
--
-- >>> Card 1 Hearts < Card 2 Hearts        -- Ord
-- True
data Card = Card Int Suit
    deriving (Eq, Ord)


-- | For instance of show typeclass
--
-- In order that "K < A" equals True,
-- consider 14 as the ace
--
-- >>> showCardNumber 14
-- "A_"
-- 
-- >>> showCardNumber 4
-- "4_"
showCardNumber :: Int -> String
showCardNumber 14 = "A_"
showCardNumber 13 = "K_"
showCardNumber 12 = "Q_"
showCardNumber 11 = "J_"
showCardNumber 10 = "10"
showCardNumber x = show x ++ "_"


-- | Show typeclass of Card
-- 
-- >>> show $ Card 1 Hearts
-- "H1_"
--
-- >>> show $ Card 14 Diamonds
-- "DA_"
--
-- >>> show $ Card 11 Clubs
-- "CJ_"
--
-- >>> show $ Card 10 Spades
-- "S10"
instance Show Card where
    show (Card i Hearts) =      "H" ++ showCardNumber i
    show (Card i Diamonds) =    "D" ++ showCardNumber i
    show (Card i Clubs) =       "C" ++ showCardNumber i
    show (Card i Spades) =      "S" ++ showCardNumber i


-- | All cards
-- 
-- >>> length allCards
-- 52
--
-- >>> take 13 $ allCards
-- [H2_,H3_,H4_,H5_,H6_,H7_,H8_,H9_,H10,HJ_,HQ_,HK_,HA_]
--
-- >>> reverse $ take 13 $ reverse allCards
-- [S2_,S3_,S4_,S5_,S6_,S7_,S8_,S9_,S10,SJ_,SQ_,SK_,SA_]
--
allCards :: [Card]
allCards = [ Card num suit | suit <- [Hearts ..], num <- [2..14] ]


-- | Get Suit from card
--
-- >>> cardSuit $ Card 10 Hearts
-- Hearts
cardSuit :: Card -> Suit
cardSuit (Card _ card) = card

-- | Get Suit from card
--
-- >>> cardNumber $ Card 10 Hearts
-- 10
cardNumber :: Card -> Int
cardNumber (Card num _) = num

-- | Stregnth of card
-- 
-- >>> cardStrength . head $ allCards
-- 2
cardStrength = cardNumber

