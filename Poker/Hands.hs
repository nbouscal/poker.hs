{-# LANGUAGE TupleSections #-}

module Poker.Hands where

import Control.Applicative
import Data.Function
import Data.List
import Data.Monoid
import Data.Ord

import Poker.Types

value :: [Card] -> Hand
value h = uncurry Hand $ maybe ifNotFlush ifFlush (getFlush h)
  where ifFlush cs = maybe (Flush, take 5 cs) (StraightFlush,) (getStraight cs)
        ifNotFlush = maybe (checkGroups h) (Straight,) (getStraight h)

getFlush :: [Card] -> Maybe [Card]
getFlush cs = if length cs' >= 5 then Just cs' else Nothing
  where groupBySuit = groupBy ((==) `on` suit)
        sortBySuit = sortBy (comparing suit <> flip compare)
        cs' = head $ sortByLength $ groupBySuit $ sortBySuit cs

getStraight :: [Card] -> Maybe [Card]
getStraight cs = if length cs'' >= 5 then Just (lastN' 5 cs'') else wheel cs'
  where cs' = nubBy ((==) `on` rank) cs
        cs'' = head $ sortByLength $ foldr f [] $ sort cs'
        f a [] = [[a]]
        f a xs@(x:xs') = if succ (rank a) == rank (head x)
                         then (a:x):xs'
                         else [a]:xs

wheel :: [Card] -> Maybe [Card]
wheel cs = if length cs' == 5 then Just cs' else Nothing
  where cs' = filter (flip elem [Ace, Two, Three, Four, Five] . rank) cs

checkGroups :: [Card] -> (HandRank, [Card])
checkGroups h = (hr, cs)
  where gs = sortByLength $ groupBy ((==) `on` rank) $ sort h
        cs = take 5 $ concat gs
        hr = case map length gs of
                  (4:_)    -> Quads
                  (3:2:_)  -> FullHouse
                  (3:_)    -> Trips
                  (2:2:_)  -> TwoPair
                  (2:_)    -> Pair
                  _        -> HighCard

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)

lastN' :: Int -> [a] -> [a]
lastN' n = foldl' (const . tail) <*> drop n
