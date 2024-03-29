module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List


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

-- GameState FilteredCards AlreadyGuessed SuitFilters ValidSuits RankFilters ValidRanks
data GameState = State [[Card]] [[Card]] [Suit] [Suit] [Rank] [Rank] | NULL

initialGuess :: Int -> ([Card],GameState)
initialGuess n = ( firstGuess, (State [] [firstGuess] (tail suitFilters) [] rankFilters [] )) 
    where rankFilters = ([minBound..maxBound]::[Rank])
          suitFilters = ([minBound..maxBound])::[Suit]
          firstGuess = sameSuitGuess n (head suitFilters)

-- next guess
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)


-- generate new deck based on valid suits and ranks
nextGuess ( (Card lsuit lrank):lastGuess, (State [] guessed [] suits [] ranks ) ) (_, _, sameRank, _, _)
    | sameRank > 0 = ( guess', (State (tail hands') (guess':guessed) [] suits [] (lrank:ranks) ) )
    | otherwise = ( guess, (State (tail hands) (guess:guessed) [] suits [] ranks ) )
    where dlen = length ( (Card lsuit lrank):lastGuess )
          hands = buildHands dlen suits ranks guessed
          hands' = buildHands dlen suits (lrank:ranks) guessed
          guess = head hands
          guess' = head hands'

-- filter through cards based on valid suits
nextGuess ( (Card lsuit lrank):lastGuess, (State [] guessed (s:suitFilters) suits rankFilters [] ) ) (_, _, _, _, suitMatches)
    | (s:suitFilters) /= [] && suitMatches > 0 = ( guess, (State [] guessed' suitFilters (lsuit:suits) rankFilters [] ))
    | (s:suitFilters) /= [] = ( guess, (State [] guessed' suitFilters suits rankFilters [] ))
    where dlen = length ((Card lsuit lrank):lastGuess)
          guess = sameSuitGuess dlen s
          guessed' = guess:guessed

-- filter cards based on rank
nextGuess ( (Card lsuit lrank):lastGuess, (State [] guessed [] suits (r:rankFilters) ranks ) ) (_, _, sameRank, highRank, suitMatches)
    | foundRank && fromSuits = ( guess, (State [] guessed' [] (lsuit:suits) rankFilters (lrank:ranks) ) )
    | fromSuits = ( guess, (State [] guessed' [] (lsuit:suits) rankFilters ranks ) )
    | foundRank = ( guess, (State [] guessed' [] suits rankFilters (lrank:ranks) ) )
    | noMoreHigher = ( guess, (State [] guessed' [] suits [] ranks ) )
    | otherwise = ( guess, (State [] guessed' [] suits rankFilters ranks ) )
    where dlen = length ((Card lsuit lrank):lastGuess)
          guess = sameRankGuess dlen r 
          guessed' = guess:guessed
          fromSuits = (ranks == []) && (suitMatches > 0)
          foundRank = sameRank > 0
          noMoreHigher = highRank == 0
          

-- search filtered hands for answer
nextGuess ( lastGuess, (State (hand:hands) guessed [] suits [] ranks ) ) _ = ( hand, (State hands guessed [] suits [] ranks ) )

------------------------------------------ Helper Functions ----------------------------------------------

-- build a new filtered deck of cards
buildHands :: Int -> [Suit] -> [Rank] -> [[Card]] -> [[Card]]
buildHands n suits ranks guessed = filterHands ( generatePotentialHands n ( generateSubDeck suits ranks ) ) guessed

-- generates a deck of cards based on available suits
generateSubDeck :: [Suit] -> [Rank] -> [Card]
generateSubDeck suits ranks = [ (Card suit rank) | suit <- suits, rank <- [minBound..maxBound]::[Rank], rank `elem` ranks ]

-- makes a guess using n number of cards with the same suit
sameSuitGuess :: Int -> Suit -> [Card]
sameSuitGuess numCards suit
    | numCards == 2 = [Card suit R2, Card suit R3]
    | numCards == 3 = [Card suit R2, Card suit R3, Card suit R4]
    | numCards == 4 = [Card suit R2, Card suit R3, Card suit R4, Card suit R5]
    | otherwise = []

-- makes a guess using n number of cards with the same suit
sameRankGuess :: Int -> Rank -> [Card]
sameRankGuess numCards rank
    | numCards == 2 = [Card Diamond rank, Card Heart rank]
    | numCards == 3 = [Card Diamond rank, Card Heart rank, Card Club rank]
    | numCards == 4 = [Card Diamond rank, Card Heart rank, Card Club rank, Card Spade rank]
    | otherwise = []


-- generates all potential hands from n number of cards
generatePotentialHands :: Int -> [Card]-> [[Card]]
generatePotentialHands numCards deck
    | numCards == 1 = [ [card] | card <- deck ]
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
    | otherwise = error "Program does not handle more than 4 cards"

-- filter out cards in first deck that exist in second deck
filterHands :: [[Card]] -> [[Card]] -> [[Card]]
filterHands _ [] = []
filterHands [] _ = []
filterHands (x:xs) ys
    | x `elem` ys = filterHands xs ys
    | otherwise = x : filterHands xs ys

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