module Game.Poker.AI 
    ( aiSelectDiscards
    , nOfKindDiscards
    ) where

import Data.List
import Data.Maybe
import Control.Applicative
import Game.Poker.Cards
import Game.Poker.Hands

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

