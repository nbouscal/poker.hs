{-# LANGUAGE TemplateHaskell, TupleSections, DeriveFunctor, FlexibleContexts #-}

import Data.Char (toLower)
import Data.List
import Data.List.Split
import Data.Ord
import Data.Monoid
import Data.Function
import Data.Traversable (traverse)
import Control.Monad.Random.Class
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import Control.Lens
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)

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
  }

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

runApp :: App a -> IO Game
runApp k = execStateT k initialState

value :: [Card] -> Hand
value h = uncurry Hand $ maybe ifNotFlush ifFlush (getFlush h)
  where ifFlush cs = maybe (Flush, take 5 cs) (StraightFlush,) (getStraight cs)
        ifNotFlush = maybe (checkGroups h) (Straight,) (getStraight h)

getFlush :: [Card] -> Maybe [Card]
getFlush cs = if length cs' >= 5 then Just cs' else Nothing
  where groupBySuit = groupBy ((==) `on` suit)
        sortBySuit = sortBy (comparing suit <> flip compare)
        cs' = head $ sortByLength $ groupBySuit $ sortBySuit cs

getStraight :: [Card] -> Maybe [Card]
getStraight cs = if length cs'' >= 5 then Just (lastN' 5 cs'') else wheel cs'
  where cs' = nubBy ((==) `on` rank) cs
        cs'' = head $ sortByLength $ foldr f [] $ sort cs'
        f a [] = [[a]]
        f a xs@(x:xs') = if succ (rank a) == rank (head x)
                         then (a:x):xs'
                         else [a]:xs

wheel :: [Card] -> Maybe [Card]
wheel cs = if length cs' == 5 then Just cs' else Nothing
  where cs' = filter (flip elem [Ace, Two, Three, Four, Five] . rank) cs

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

advance :: MonadState Game m => m ()
advance = do
  s <- use street
  clearBets
  case s of
       PreDeal -> nextStreet >> dealPlayers 2
       PreFlop -> nextStreet >> dealCommunity 3
       Flop -> nextStreet >> dealCommunity 1
       Turn -> nextStreet >> dealCommunity 1
       River -> street .= minBound >> deck .= initialState^.deck
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



unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (`unless` s)

betting :: (MonadState Game m, MonadIO m) => m ()
betting = unlessM bettingDone $ bettingRound >> betting

showBets :: (MonadState Game m, MonadIO m) => m ()
showBets = use players >>= liftIO . print . map (view bet &&& view chips)

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

betOrFold :: (MonadState Game m, MonadIO m) => Player -> m Player
betOrFold p = do
  mb <- use maxBet
  b <- getBetOrFold mb
  let d = max 0 $ toInt b - toInt (p^.bet)
  maxBet .= max b mb >> pot += d
  return $ chips -~ d $ bet .~ b $ p

checkOrBet :: (MonadState Game m, MonadIO m) => Player -> m Player
checkOrBet p =  do
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



initialState :: Game
initialState = Game
  { _players = replicate 5 player
  , _community = []
  , _deck = Card <$> [minBound..] <*> [minBound..]
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

showGame :: (MonadState Game m, MonadIO m) => m ()
showGame = do
  ps <- use players
  cs <- use community
  let hs = map ((value . (++cs) &&& id) . view pockets) ps
      ws = maximums hs
      showCards = foldl (\a c -> a ++ " " ++ show c) "\t"
      showHands = foldl (\a (h, cs) -> a ++ showCards cs ++ " – " ++ show (h^.handRank) ++ "\n") ""
  liftIO $ putStr $ "Hands:\n" ++ showHands hs ++ "Community:\n" ++ showCards cs ++
    (if length ws == 1 then "\nWinner:\n" else "\nWinners:\n") ++ showHands ws

main :: IO Game
main = runApp play
