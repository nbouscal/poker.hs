{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Poker.Game
  ( playStreet
  , initialDeck
  , winners
  , shuffle
  ) where

------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens               hiding (Fold)
import           Control.Lens.Action
import           Control.Monad.Random.Class
import           Control.Monad.State        hiding (state)
import           Data.List.Split
import           System.Random.Shuffle      (shuffleM)
------------------------------------------------------------------------------
import           Poker.Hands
import           Poker.Types
import           Poker.Utility
------------------------------------------------------------------------------

initialDeck :: [Card]
initialDeck = Card <$> [minBound..] <*> [minBound..]

playStreet :: MonadState Game m => m ()
playStreet = do
  s <- use street
  clearBets
  clearState
  nextStreet s
  case s of
       PreDeal -> dealPlayers 2
       PreFlop -> dealCommunity 3
       Flop    -> dealCommunity 1
       Turn    -> dealCommunity 1
       River   -> finishHand

clearBets :: MonadState Game m => m ()
clearBets = maxBet .= 0 >> players.traversed.bet .= 0

clearState :: MonadState Game m => m ()
clearState = players.traversed.filtered notOut.state .= None

nextStreet :: MonadState Game m => Street -> m ()
nextStreet River = street .= minBound
nextStreet _     = street %= succ

-- who is dealer? last in array => rotate array each hand?
--                add a _dealer to game, index of players?
dealPlayers :: MonadState Game m => Int -> m ()
dealPlayers n = do
  m <- uses players length
  d <- use deck
  let (hs, d') = first (chunksOf n) $ splitAt (m * n) d
  players.traversed %@= (\i -> pockets <>~ (hs !! i))
  deck .= d'

dealCommunity :: MonadState Game m => Int -> m ()
dealCommunity n = do
  d <- use deck
  let (x,y) = splitAt n d
  community <>= x
  deck .= y

finishHand :: MonadState Game m => m ()
finishHand = do
  deck .= initialDeck
  winners >>= splitPot
  community .= []
  players.traversed.state .= None
  players.traversed.committed .= 0
  players.traversed.pockets .= []

shuffle :: (MonadState Game m, MonadRandom m) => m ()
shuffle = deck <~ (get >>= perform (deck.act shuffleM))

winners :: MonadState Game m => m [Player]
winners = do
  cs <- use community
  ps <- liftM (filter (\p -> p^.state /= Out Fold)) (use players)
  let ws = maximums $ map (value . (++cs) . view pockets &&& id) ps
  return $ ws^..folded._2

splitPot :: MonadState Game m => [Player] -> m ()
splitPot ps = do
  p <- use pot
  let n = length ps
      w = p `div` n
  pot .= p `rem` n
  players.traversed.filtered (`elem` ps).chips += w
