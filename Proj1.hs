--module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

-- game state takes _ parameters:
-- Once bottom and top range is established as well as invalid suits, the algorithm
-- will generate all potential cards and then search them

-- [Card] -> all the potential cards
-- Filters -> holds range and suit values to filter on, once all are empty then card generation can begin
-- ValidSuits -> Valid suits 
-- ValidRange -> Valid Range of cards
-- Bool -> initial validation complete

-- Bool -> answer is correct, exit game 
data GameState = GameState [Card] Filters Bool ValidSuits ValidRange | Empty

data Filters = Filters [Rank] [Suit]
data ValidRange = ValidRange Rank Rank
data ValidSuits = ValidSuits [Suit]

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] [] = (0,0,0,0,0)
feedback x [] = (0,0,0,0,0)
feedback [] x = (0,0,0,0,0)
feedback answers guesses = ( 
    numCorrectCards guesses answers,
    cardsWithLowerRank guesses answers,
    cardsWithSameRank guesses answers,
    cardsWithHigherRank guesses answers,
    cardsWithSameSuit guesses answers 
    )


initialGuess :: Int -> ([Card],GameState)
initialGuess 0 = ([], Empty)
initialGuess x
    | x == 2 = ([ Card Club R2, Card Club R3 ], 
               (GameState 
                    [] 
                    ( Filters ([minBound..maxBound]::[Rank]) ([minBound..maxBound]::[Suit]) ) 
                    False 
                    ( ValidSuits ([minBound..maxBound]::[Suit]) )
                    ( ValidRange R2 Ace )
                ))
    | otherwise = ([], Empty)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess ( 
            cards, 
            ( GameState potentialCards (Filters franks fsuits) isValidated (ValidSuits suits) (ValidRange low high) ) 
          )  (matchs, lowRank, sameRank, highRank, sameSuit ) = ([], Empty)


generatePotentialHands :: Int -> [[Card]]
generatePotentialHands numCards 
    | numCards == 2 = [ [card]++[card'] | card <- deck, card' <- deck, card /= card' ]
    | numCards == 3 = [ [card]++[card']++[card''] 
                        | card <- deck
                        , card' <- deck
                        , card'' <- deck
                        , card /= card' 
                        , card' /= card''
                        , card /= card'' 
                    ]
    | numCards == 4 = [ [card]++[card']++[card'']++[card4]
                        | card <- deck
                        , card' <- deck
                        , card'' <- deck
                        , card4 <- deck
                        , card /= card' 
                        , card' /= card''
                        , card /= card'' 
                        , card4 /= card
                        , card4 /= card' 
                        , card4 /= card''
                    ]
    | otherwise = [[]]
    where deck = [minBound..maxBound]::[Card]


-- recieves current guess and answer hand, determines how many cards are found
numCorrectCards :: [Card] -> [Card] -> Int
numCorrectCards xs ys = length [ x | x <- xs, y <- ys, x == y ]

-- Counts how many guess cards have the same rank as the answer cards
cardsWithSameRank :: [Card] -> [Card] -> Int
cardsWithSameRank [] [] = 0
cardsWithSameRank x [] = 0
cardsWithSameRank [] x = 0
cardsWithSameRank guesses answers = countPairs [ rank | (Card _ rank) <- answers ] [ rank | (Card _ rank) <- guesses ]

-- returns number of cards from answer with lower rank than lowest ranking card in guesses
-- takes in guesses as first parameter and answers as second parameter
cardsWithLowerRank :: [Card] -> [Card] -> Int
cardsWithLowerRank _ [] = 0
cardsWithLowerRank guesses ((Card _ rank) : answers)
    | rank < lowestRank(guesses) = 1 + cardsWithLowerRank guesses answers 
    | otherwise = cardsWithLowerRank guesses answers

-- return lowest rank from list of cards
lowestRank :: [Card] -> Rank
lowestRank [(Card _ rank)] = rank
lowestRank ( ( Card xsuit xrank ) : ( ( Card ysuit yrank ) : ys ) )
    | yrank < xrank = lowestRank ( ( Card ysuit yrank ) : ys )
    | otherwise = lowestRank ( ( Card xsuit xrank ) : ys )


-- returns number of cards from answer with higher rank than highest ranking card in guesses
-- takes in guesses as first parameter and answers as second parameter
cardsWithHigherRank :: [Card] -> [Card] -> Int
cardsWithHigherRank _ [] = 0
cardsWithHigherRank guesses ((Card _ rank) : answers)
    | rank > highestRank(guesses) = 1 + cardsWithHigherRank guesses answers 
    | otherwise = cardsWithHigherRank guesses answers

-- return highest rank from list of cards
highestRank :: [Card] -> Rank
highestRank [(Card _ rank)] = rank
highestRank ( ( Card xsuit xrank ) : ( ( Card ysuit yrank ) : ys ) )
    | yrank > xrank = highestRank ( ( Card ysuit yrank ) : ys )
    | otherwise = highestRank ( ( Card xsuit xrank ) : ys )


-- number of cards with same suit
cardsWithSameSuit :: [Card] -> [Card] -> Int
cardsWithSameSuit [] [] = 0
cardsWithSameSuit x [] = 0
cardsWithSameSuit [] x = 0
cardsWithSameSuit guesses answers = countPairs [ suit | (Card suit _) <- answers ] [ suit | (Card suit _) <- guesses ]


-- iterate through two lists and count the pairs
countPairs :: (Eq a) => [a] -> [a] -> Int
countPairs [] [] = 0
countPairs x [] = 0
countPairs [] x = 0
countPairs (x:xs) ys 
    | x `elem` ys = 1 + countPairs xs (delete x ys)
    | otherwise = countPairs xs ys