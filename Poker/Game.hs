{-# LANGUAGE FlexibleContexts #-}

module Poker.Game where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State
import Data.Char (toLower)
import Data.List.Split
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)

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
  players.traversed.(filtered (`elem` ps)).chips += w

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (`unless` s)

betting :: (MonadState Game m, MonadIO m) => m ()
betting = unlessM bettingDone $ bettingRound >> betting

bettingDone :: MonadState Game m => m Bool
bettingDone = do
  mb <- use maxBet
  liftM (all $ f mb) $ use players
  where f mb p = case p^.bet of
                      None -> False
                      Fold -> True
                      _    -> p^.bet == mb

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
  let state = "Pockets: " ++ (show $ p^.pockets) ++ " Community: " ++ (show c) ++ " Bet: " ++ (show $ toInt mb)
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
  input <- liftIO $ getLine
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
  input <- liftIO $ getLine
  case map toLower input of
       "check" -> return Check
       "bet"   -> liftIO $ putStrLn "Bet how much?" >> fmap Bet getBet
       _       -> getCheckOrBet

getBet :: MonadIO m => m Int
getBet = liftIO getLine >>= maybe (liftIO $ putStrLn "Invalid bet" >> getBet) return . readMaybe
