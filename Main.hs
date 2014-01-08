{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow
import Control.Lens
import Control.Monad.State

import Poker.Betting
import Poker.Game
import Poker.Hands
import Poker.Types

initialState :: Game
initialState = Game
  { _players = replicate 5 player
  , _community = []
  , _deck = initialDeck
  , _pot = 0
  , _street = PreDeal
  , _maxBet = None
  }
  where player = Player
          { _pockets = []
          , _chips = 1500
          , _bet = None
          }

play :: App ()
play = do
  shuffle
  replicateM_ 4 (advance >> betting)
  showGame

showBets :: (MonadState Game m, MonadIO m) => m ()
showBets = use players >>= liftIO . print . map (view bet &&& view chips)

showGame :: (MonadState Game m, MonadIO m) => m ()
showGame = do
  ps <- use players
  cs <- use community
  ws <- winners
  let getHands = map ((value . (++cs) &&& id) . view pockets)
      hs = getHands ps
      showCards = foldl (\a c -> a ++ " " ++ show c) "\t"
      showHands = foldl (\a (h, cs) -> a ++ showCards cs ++ " – " ++ show (h^.handRank) ++ "\n") ""
  liftIO $ putStr $ "Hands:\n" ++ showHands hs ++ "Community:\n" ++ showCards cs ++
    (if length ws == 1 then "\nWinner:\n" else "\nWinners:\n") ++ showHands (getHands ws)

main :: IO Game
main = execStateT play initialState
