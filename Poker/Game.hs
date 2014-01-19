{-# LANGUAGE FlexibleContexts #-}

module Poker.Game (
  advance, initialDeck, winners, shuffle
) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State
import Data.List.Split
import System.Random.Shuffle (shuffleM)

import Poker.Types
import Poker.Hands
import Poker.Utility

initialDeck = Card <$> [minBound..] <*> [minBound..]

advance :: MonadState Game m => m ()
advance = do
  s <- use street
  clearBets
  nextStreet s
  case s of
       PreDeal -> dealPlayers 2
       PreFlop -> dealCommunity 3
       Flop -> dealCommunity 1
       Turn -> dealCommunity 1
       River -> deck .= initialDeck >> winners >>= splitPot

clearBets :: MonadState Game m => m ()
clearBets = maxBet .= None >> players.traversed.bet %= clearUnlessFold
  where clearUnlessFold Fold = Fold
        clearUnlessFold _    = None

nextStreet :: MonadState Game m => Street -> m ()
nextStreet s = if s == River
               then street .= minBound
               else street %= succ

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
