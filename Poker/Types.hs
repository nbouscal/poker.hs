{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Poker.Types where

import Control.Lens
import Control.Monad.State
import Data.Function

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
  deriving (Eq, Ord, Bounded, Enum)
instance Show Rank where
  show x = case x of
    Two   -> "2"
    Three -> "3"
    Four  -> "4"
    Five  -> "5"
    Six   -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine  -> "9"
    Ten   -> "T"
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Bounded, Enum)
instance Show Suit where
  show x = case x of
    Clubs    -> "♧ "
    Diamonds -> "♢ "
    Hearts   -> "♡ "
    Spades   -> "♤ "

data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving Eq
instance Ord Card where
  compare = compare `on` rank
instance Show Card where
  show (Card r s) = show r ++ show s

data HandRank = HighCard | Pair | TwoPair | Trips | Straight | Flush
              | FullHouse | Quads | StraightFlush
  deriving (Eq, Ord, Show)

data GenericBet a = None | Fold | Check | Bet a
  deriving (Eq, Ord, Show, Functor)
type Bet = GenericBet Int

data Hand = Hand
  { _handRank :: HandRank
  , _cards :: [Card]
  } deriving (Eq, Ord)

data Player = Player
  { _pockets :: [Card]
  , _chips :: Int
  , _bet :: Bet
  } deriving Eq

data Game = Game
  { _players :: [Player]
  , _community :: [Card]
  , _deck :: [Card]
  , _street :: Street
  , _pot :: Int
  , _maxBet :: Bet
  }

data Street = PreDeal | PreFlop | Flop | Turn | River
  deriving (Eq, Ord, Show, Bounded, Enum)

makeLenses ''Hand
makeLenses ''Player
makeLenses ''Game

type App = StateT Game IO
