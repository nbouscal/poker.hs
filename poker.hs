{-# LANGUAGE TemplateHaskell #-}

import Data.List
import Data.List.Split
import Data.Ord
import Data.Monoid
import Data.Function
import Control.Monad.Random.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Applicative
import Control.Arrow
import Control.Lens
import System.Random.Shuffle (shuffleM)
import System.IO

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
  compare (Card r1 s1) (Card r2 s2) = compare r1 r2
instance Show Card where
  show (Card r s) = show r ++ show s

data HandRank = HighCard | Pair | TwoPair | Trips | Straight | Flush
              | FullHouse | Quads | StraightFlush
  deriving (Eq, Ord, Show)

data Hand = Hand
  { _handRank :: HandRank
  , _cards :: [Card]
  } deriving (Eq, Ord)

data Player = Player
  { _pockets :: [Card]
  , _chips :: Int
  }

data Game = Game
  { _players :: [Player]
  , _community :: [Card]
  , _deck :: [Card]
  , _street :: Street
  , _pot :: Int
  }

data Street = PreDeal | PreFlop | Flop | Turn | River
  deriving (Eq, Ord, Show, Bounded, Enum)

makeLenses ''Hand
makeLenses ''Player
makeLenses ''Game

value :: [Card] -> Hand
value h = Hand hr cs
  where (hr, cs) = case getFlush h of
                        Just cs -> case getStraight cs of
                                        Just cs' -> (StraightFlush, cs')
                                        Nothing  -> (Flush, take 5 cs)
                        Nothing -> case getStraight h of
                                        Just cs -> (Straight, cs)
                                        Nothing -> checkGroups h

getFlush :: [Card] -> Maybe [Card]
getFlush cs = if length cs' >= 5
              then Just cs'
              else Nothing
  where groupBySuit = groupBy ((==) `on` suit)
        sortBySuit = sortBy (comparing suit <> flip compare)
        cs' = head $ sortByLength $ groupBySuit $ sortBySuit cs

getStraight :: [Card] -> Maybe [Card]
getStraight cs = if length cs' >= 5
                 then Just (lastN' 5 cs')
                 else Nothing
  where cs' = head $ sortByLength $ foldr f [] $ sort $ nubBy ((==) `on` rank) cs
        f a [] = [[a]]
        f a xs@(x:xs') = if succ (rank a) == rank (head x)
                         then (a:x):xs'
                         else [a]:xs

checkGroups :: [Card] -> (HandRank, [Card])
checkGroups h = (hr, cs)
  where gs = sortByLength $ groupBy ((==) `on` rank) $ sort h
        cs = take 5 $ concat gs
        hr = case map length gs of
                  (4:_)    -> Quads
                  (3:2:_)  -> FullHouse
                  (3:_)    -> Trips
                  (2:2:_)  -> TwoPair
                  (2:_)    -> Pair
                  _        -> HighCard

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)

lastN' :: Int -> [a] -> [a]
lastN' n = foldl' (const . tail) <*> drop n

maximums :: Ord a => [(a,b)] -> [(a,b)]
maximums [] = []
maximums (x:xs) = foldl f [x] xs
  where f xs y = case compare (fst $ head xs) (fst y) of
                      GT -> xs
                      EQ -> y:xs
                      LT -> [y]

advance :: StateT Game IO ()
advance = do
  s <- use street
  case s of
       PreDeal -> nextStreet >> dealPlayers 2
       PreFlop -> nextStreet >> dealCommunity 3
       Flop -> nextStreet >> dealCommunity 1
       Turn -> nextStreet >> dealCommunity 1
       River -> street .= minBound
  where nextStreet = street %= succ

dealCommunity :: Int -> StateT Game IO ()
dealCommunity n = use deck >>=
  uncurry (>>) . bimap (community <>=) (deck .=) . splitAt n

dealPlayers :: Int -> StateT Game IO ()
dealPlayers n = do
  m <- uses players length
  d <- use deck
  let (hs, d') = first (chunksOf n) $ splitAt (m * n) d
  players.traversed %@= (\i -> pockets <>~ (hs !! i))
  deck .= d'

shuffle :: StateT Game IO ()
shuffle = get >>= (^!deck.act shuffleM) >>= (deck .=)

initialState :: Game
initialState = Game
  { _players = replicate 5 player
  , _community = []
  , _deck = Card <$> [minBound..] <*> [minBound..]
  , _pot = 0
  , _street = PreDeal
  }
  where player = Player
          { _pockets = []
          , _chips = 1500
          }

play :: StateT Game IO ()
play = do
  shuffle
  advance
  advance
  advance
  advance
  showGame

showGame :: StateT Game IO ()
showGame = do
  ps <- use players
  cs <- use community
  let hs = map ((value . (++cs) &&& id) . (^.pockets)) ps
      ws = maximums hs
      showCards = foldl (\a c -> a ++ " " ++ show c) "\t"
      showHands = foldl (\a (h, cs) -> a ++ showCards cs ++ " – " ++ show (h^.handRank) ++ "\n") ""
  lift $ putStr $ "Hands:\n" ++ showHands hs ++ "Community:\n" ++ showCards cs ++
    (if length ws == 1 then "\nWinner:\n" else "\nWinners:\n") ++ showHands ws

main :: IO Game
main = execStateT play initialState
