module Poker.Utility where

import Control.Lens
import Control.Monad.State hiding (state)

import Poker.Types

notOut :: Player -> Bool
notOut p = not $ isOut $ p^.state

maximums :: Ord a => [(a,b)] -> [(a,b)]
maximums [] = []
maximums (x:xs) = foldl f [x] xs
  where f xs y = case fst (head xs) `compare` fst y of
                      GT -> xs
                      EQ -> y:xs
                      LT -> [y]

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (`unless` s)

putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn
