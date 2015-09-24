{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Loops
------------------------------------------------------------------------------
import           Poker.Betting
import           Poker.Game
import           Poker.Hands
import           Poker.Types
------------------------------------------------------------------------------

initialState :: Game
initialState = Game
  { _players = replicate 5 player
  , _community = []
  , _deck = initialDeck
  , _pot = 0
  , _street = PreDeal
  , _maxBet = 0
  }
  where player = Player
          { _pockets = []
          , _chips = 1500
          , _bet = 0
          , _state = None
          , _committed = 0
          }

playGame :: App ()
playGame = playHand >> playGame

playHand :: App ()
playHand = do
  shuffle
  whileM (uses street (< River)) (playStreet >> betting)
  showGame
  playStreet
  showPlayers

showBets :: (MonadState Game m, MonadIO m) => m ()
showBets = use players >>= liftIO . print . map (view bet &&& view chips)

showGame :: (MonadState Game m, MonadIO m) => m ()
showGame = do
  ps <- use players
  com <- use community
  ws <- winners
  let getHands = map ((value . (++com) &&& id) . view pockets)
      hs = getHands ps
      showCards = foldl (\a c -> a ++ " " ++ show c) "\t"
      showHands = foldl (\a (h, cs) -> a ++ showCards cs ++ " – " ++ show (fst h) ++ "\n") ""
  liftIO $ putStrLn $ "Hands:\n" ++ showHands hs ++ "Community:\n" ++ showCards com ++
    (if length ws == 1 then "\nWinner:\n" else "\nWinners:\n") ++ showHands (getHands ws)

showPlayers :: (MonadState Game m, MonadIO m) => m ()
showPlayers = join $ liftM (liftIO . putStrLn . concatMap show) (use players)

main :: IO Game
main = execStateT playGame initialState
