module Game.Poker.Simple
    (simpleGame
    ) where

import Control.Monad
import System.Random.Shuffle
import Game.Poker.Cards    -- Fuda
import Game.Poker.Hands    -- Tefuda
import Game.Poker.AI       -- core AI of CPU

data Player = Player | Enemy deriving Eq

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

