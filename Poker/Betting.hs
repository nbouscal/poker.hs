{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Poker.Betting (
  betting
) where

import Control.Lens
import Control.Monad.State hiding (state)
import Data.Char (toLower)
import Text.Read (readMaybe)

import Poker.Types
import Poker.Hands
import Poker.Utility

betting :: (MonadState Game m, MonadIO m) => m ()
betting = unlessM bettingDone $ bettingRound >> betting

bettingDone :: MonadState Game m => m Bool
bettingDone = do
  mb <- use maxBet
  ps <- use players
  let actives = ps^..folded.filtered notOut
      actives' = filter notOut ps
  return $ allOf (folded.bet) (== mb) actives &&
    ((length actives <= 1) ||
    allOf (folded.state) (/= None) actives)

bettingRound :: (MonadState Game m, MonadIO m) => m ()
bettingRound = players <~ (get >>= perform (players.traversed.act playerAction))

playerAction :: (MonadState Game m, MonadIO m) => Player -> m [Player]
playerAction p = do
  mb <- use maxBet
  liftM return $ p & case p^.state of
    None -> getAction (p^.bet == mb)
    Out _ -> return
    In -> if p^.bet < mb
          then getAction False
          else return

showState :: (MonadState Game m, MonadIO m) => Player -> m ()
showState p = do
  c <- use community
  mb <- use maxBet
  let state = "Pockets: " ++ show (p^.pockets)
           ++ " Community: " ++ show c
           ++ " Bet: " ++ show mb
           ++ " Chips: " ++ show (p^.chips)
  putIO state

getAction :: (MonadState Game m, MonadIO m) => Bool -> Player -> m Player
getAction canBet p = do
  showState p
  mb <- use maxBet
  (s, b) <- if canBet then getCheckOrBet else getBetOrFold mb
  let d = max 0 $ b - (p^.bet)
      makeBet = (chips -~ d) . (bet +~ d) . (committed +~ d)
      update s = do maxBet .= max b mb
                    pot += d
                    return $ state .~ s $ makeBet p
  case d `compare` (p^.chips) of
       GT -> putIO "You don't have that many chips." >> getAction canBet p
       EQ -> update $ Out AllIn
       LT -> update s

getBetOrFold :: MonadIO m => Bet -> m (PlayerState, Bet)
getBetOrFold mb = do
  putIO "Fold, Call, or Raise?"
  input <- liftIO getLine
  case map toLower input of
       "fold"  -> return (Out Fold, 0)
       "call"  -> return (In, mb)
       "raise" -> liftM (In,) (getRaise mb)
       _       -> getBetOrFold mb

getCheckOrBet :: MonadIO m => m (PlayerState, Bet)
getCheckOrBet = do
  putIO "Check or Bet?"
  input <- liftIO getLine
  case map toLower input of
       "check" -> return (In, 0)
       "bet"   -> putIO "Bet how much?" >> liftM (In,) getBet
       _       -> getCheckOrBet

getRaise :: MonadIO m => Bet -> m Bet
getRaise mb = putIO "Raise by how much?" >> liftM (+mb) getBet

getBet :: MonadIO m => m Int
getBet = liftIO getLine >>= maybe (putIO "Invalid bet" >> getBet) return . readMaybe
