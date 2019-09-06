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

-- GameState FilteredCards AlreadyGuessed SuitFilters ValidSuits FinalDeck
data GameState = State [[Card]] [[Card]] [Suit] [Suit] [[Card]] | NULL

initialGuess :: Int -> ([Card],GameState)
initialGuess n = ( (sameSuitGuess n Diamond), (State [] [sameSuitGuess n Diamond] [Club, Spade, Heart] [] [] ))


-- next guess
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)

-- filter through cards based on valid suits
nextGuess ( (Card lsuit lrank):lastGuess, (State [] guessed (s:suitFilters) suits [] ) ) (_, _, _, _, suitMatches)
    | (s:suitFilters) /= [] && suitMatches > 0 = ( guess, (State [] guessed' suitFilters (lsuit:suits) [] ))
    | (s:suitFilters) /= [] = ( guess, (State [] guessed' suitFilters suits [] ))
    where dlen = length ((Card lsuit lrank):lastGuess)
          guess = sameSuitGuess dlen s
          guessed' = guess:guessed

-- generate new deck based on available suits
nextGuess ( (Card lsuit lrank):lastGuess, (State [] guessed [] suits [] ) ) (_, _, _, _, suitMatches)
    | suitMatches > 0 = ( guess', (State (tail hands') (guess':guessed) [] suits [] ) )
    | otherwise = ( guess, (State (tail hands) (guess:guessed) [] suits [] ) )
    where dlen = length ( (Card lsuit lrank):lastGuess )
          hands = buildHands dlen suits guessed
          hands' = buildHands dlen (lsuit:suits) guessed
          guess = head hands
          guess' = head hands'

-- loop through that deck, if match is found then build out new groups of hands containing those hands
-- and check them for matches, removing each set if no match is found
-- feature only working for 2 card hands
nextGuess ( lastGuess, (State (hand:hands) guessed [] suits [] ) ) (matches,_,_,_,_) 
    | matches > 0 = (hand, ( State [] [] [] [] (filterHands deck guessed) ))
    | otherwise = ( hand, (State hands guessed [] suits [] ) )
    where dlen = length lastGuess
          subDeck = generatePotentialHands 1 ( generateSubDeck suits )
          deck = [ [(head lastGuess)]++hand | hand <- subDeck, not ((head lastGuess) `elem` hand) ] ++ [ [(last lastGuess)]++hand' | hand' <- subDeck, not ( (last lastGuess) `elem` hand' ) ]
           


nextGuess ( lastGuess, ( State _ _ _ _ (f:final) ) ) _ = ( f, ( State [] [] [] [] final ) )

------------------------------------------ Helper Functions ----------------------------------------------

-- build a new filtered deck of cards
buildHands :: Int -> [Suit] -> [[Card]] -> [[Card]]
buildHands n suits guessed = filterHands ( generatePotentialHands n ( generateSubDeck suits ) ) guessed

-- generates a deck of cards based on available suits
generateSubDeck :: [Suit] -> [Card]
generateSubDeck suits = [ (Card suit rank) | suit <- suits, rank <- [minBound..maxBound]::[Rank] ]

-- makes a guess using n number of cards with the same suit
sameSuitGuess :: Int -> Suit -> [Card]
sameSuitGuess numCards suit
    | numCards == 2 = [Card suit R2, Card suit R3]
    | numCards == 3 = [Card suit R2, Card suit R3, Card suit R4]
    | numCards == 4 = [Card suit R2, Card suit R3, Card suit R4, Card suit R5]
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