{-# LANGUAGE TemplateHaskell #-}

module Poker.Types where

import Control.Lens
import Control.Monad.State hiding (state)
import Data.DeriveTH
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

type Bet = Int

data Out = Fold | AllIn
  deriving (Eq, Show)

data PlayerState = None | Out Out | In
  deriving (Eq, Show)

data Street = PreDeal | PreFlop | Flop | Turn | River
  deriving (Eq, Ord, Show, Bounded, Enum)

data Hand = Hand
  { handRank :: HandRank
  , cards :: [Card]
  } deriving (Eq, Ord)

data Player = Player
  { _pockets :: [Card]
  , _chips :: Int
  , _bet :: Bet
  , _state :: PlayerState
  , _committed :: Bet
  } deriving (Eq, Show)

data Game = Game
  { _players :: [Player]
  , _community :: [Card]
  , _deck :: [Card]
  , _street :: Street
  , _pot :: Int
  , _maxBet :: Bet
  }

$( derive makeIs ''PlayerState)

makeLenses ''Player
makeLenses ''Game

type App = StateT Game IO
