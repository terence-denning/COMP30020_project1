--module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card

--feedback :: [Card] → [Card] → (Int,Int,Int,Int,Int)



--initialGuess :: Int → ([Card],GameState)


--nextGuess :: ([Card],GameState) → (Int,Int,Int,Int,Int) → ([Card],GameState)


-- recieves current guess and answer hand, determines how many cards are found
numCorrectCards :: [Card] -> [Card] -> Int
numCorrectCards xs ys = length [ x | x <- xs, y <- ys, x == y ]

-- Counts how many guess cards have the same rank as the answer cards
cardsWithSameRank :: [Card] -> [Card] -> Int
cardsWithSameRank [] [] = 0
cardsWithSameRank x [] = 0
cardsWithSameRank [] x = 0
cardsWithSameRank guesses answers = length [ rank | (Card _ rank) <- guesses, rank `elem` [ rank | (Card _ rank) <- answers ] ] 

-- returns number of cards from answer with lower rank than lowest ranking card in guesses
-- takes in guesses as first parameter and answers as second parameter
cardsWithLowestRank :: [Card] -> [Card] -> Int
cardsWithLowestRank _ [] = 0
cardsWithLowestRank guesses ((Card _ rank) : answers)
    | rank < lowestRank(guesses) = 1 + cardsWithLowestRank guesses answers 
    | otherwise = cardsWithLowestRank guesses answers

-- return lowest rank from list of cards
lowestRank :: [Card] -> Rank
lowestRank [(Card _ rank)] = rank
lowestRank ( ( Card xsuit xrank ) : ( ( Card ysuit yrank ) : ys ) )
    | yrank < xrank = lowestRank ( ( Card ysuit yrank ) : ys )
    | otherwise = lowestRank ( ( Card xsuit xrank ) : ys )
