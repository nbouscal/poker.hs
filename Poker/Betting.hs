{-# LANGUAGE FlexibleContexts #-}

module Poker.Betting where

import Control.Lens
import Control.Monad.State
import Data.Char (toLower)
import Text.Read (readMaybe)

import Poker.Types
import Poker.Hands

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (`unless` s)

betting :: (MonadState Game m, MonadIO m) => m ()
betting = unlessM bettingDone $ bettingRound >> betting

bettingDone :: MonadState Game m => m Bool
bettingDone = do
  mb <- use maxBet
  ps <- use players
  let bs = map (view bet) ps
      actives = filter (/= Fold) bs
  return ((length actives == 1) || (all $ f mb) bs)
  where f mb b = case b of
                      None -> False
                      Fold -> True
                      _    -> b == mb

bettingRound :: (MonadState Game m, MonadIO m) => m ()
bettingRound = players <~ (get >>= perform (players.traversed.act playerAction))

playerAction :: (MonadState Game m, MonadIO m) => Player -> m [Player]
playerAction p = do
  mb <- use maxBet
  let b = p^.bet
  liftM return $ p & case mb of
    (Bet _) | b == Fold -> return
            | b < mb    -> betOrFold
    _       | b == None -> checkOrBet
            | otherwise -> return

toInt :: Bet -> Int
toInt (Bet x) = x
toInt _       = 0

showState :: (MonadState Game m, MonadIO m) => Player -> m ()
showState p = do
  c <- use community
  mb <- use maxBet
  let state = "Pockets: " ++ show (p^.pockets) ++ " Community: " ++ show c ++ " Bet: " ++ show (toInt mb)
  liftIO $ putStrLn state

betOrFold :: (MonadState Game m, MonadIO m) => Player -> m Player
betOrFold p = do
  showState p
  mb <- use maxBet
  b <- getBetOrFold mb
  let d = max 0 $ toInt b - toInt (p^.bet)
  maxBet .= max b mb >> pot += d
  return $ chips -~ d $ bet .~ b $ p

checkOrBet :: (MonadState Game m, MonadIO m) => Player -> m Player
checkOrBet p =  do
  showState p
  b <- getCheckOrBet
  let d = toInt b
  maxBet .= b >> pot += d
  return $ chips -~ d $ bet .~ b $ p

getBetOrFold :: MonadIO m => Bet -> m Bet
getBetOrFold mb = do
  liftIO $ putStrLn "Fold, Call, or Raise?"
  input <- liftIO getLine
  case map toLower input of
       "fold"  -> return Fold
       "call"  -> return mb
       "raise" -> getRaise mb
       _       -> getBetOrFold mb

getRaise :: MonadIO m => Bet -> m Bet
getRaise mb = liftIO $ putStrLn "Raise by how much?" >> liftM (\r -> fmap (+r) mb) getBet

getCheckOrBet :: MonadIO m => m Bet
getCheckOrBet = do
  liftIO $ putStrLn "Check or Bet?"
  input <- liftIO getLine
  case map toLower input of
       "check" -> return Check
       "bet"   -> liftIO $ putStrLn "Bet how much?" >> fmap Bet getBet
       _       -> getCheckOrBet

getBet :: MonadIO m => m Int
getBet = liftIO getLine >>= maybe (liftIO $ putStrLn "Invalid bet" >> getBet) return . readMaybe
