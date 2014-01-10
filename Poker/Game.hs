{-# LANGUAGE FlexibleContexts #-}

module Poker.Game where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State
import Data.List.Split
import System.Random.Shuffle (shuffleM)

import Poker.Types
import Poker.Hands

initialDeck = Card <$> [minBound..] <*> [minBound..]

maximums :: Ord a => [(a,b)] -> [(a,b)]
maximums [] = []
maximums (x:xs) = foldl f [x] xs
  where f xs y = case compare (fst $ head xs) (fst y) of
                      GT -> xs
                      EQ -> y:xs
                      LT -> [y]

advance :: MonadState Game m => m ()
advance = do
  s <- use street
  clearBets
  case s of
       PreDeal -> nextStreet >> dealPlayers 2
       PreFlop -> nextStreet >> dealCommunity 3
       Flop -> nextStreet >> dealCommunity 1
       Turn -> nextStreet >> dealCommunity 1
       River -> street .= minBound >> deck .= initialDeck >>
                winners >>= splitPot
  where nextStreet = street %= succ
        clearUnlessFold Fold = Fold
        clearUnlessFold _    = None
        clearBets = maxBet .= None >> players.traversed.bet %= clearUnlessFold

dealCommunity :: MonadState Game m => Int -> m ()
dealCommunity n = use deck >>=
  uncurry (>>) . bimap (community <>=) (deck .=) . splitAt n

-- who is dealer? last in array => rotate array each hand?
--                add a _dealer to game, index of players?
dealPlayers :: MonadState Game m => Int -> m ()
dealPlayers n = do
  m <- uses players length
  d <- use deck
  let (hs, d') = first (chunksOf n) $ splitAt (m * n) d
  players.traversed %@= (\i -> pockets <>~ (hs !! i))
  deck .= d'

shuffle :: (MonadState Game m, MonadRandom m) => m ()
shuffle = deck <~ (get >>= perform (deck.act shuffleM))

winners :: MonadState Game m => m [Player]
winners = do
  ps <- use players
  cs <- use community
  let ps' = filter (\p -> p^.bet /= Fold) ps
      hs = map (value . (++cs) . view pockets &&& id) ps'
      ws = maximums hs
  return $ ws^..traversed._2

splitPot :: MonadState Game m => [Player] -> m ()
splitPot ps = do
  p <- use pot
  let n = length ps
      w = p `div` n
  pot .= p `rem` n
  players.traversed.filtered (`elem` ps).chips += w
