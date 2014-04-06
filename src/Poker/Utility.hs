module Poker.Utility where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State hiding (state)
------------------------------------------------------------------------------
import           Poker.Types
------------------------------------------------------------------------------

notOut :: Player -> Bool
notOut p = not $ isOut $ p^.state

maximums :: Ord a => [(a,b)] -> [(a,b)]
maximums [] = []
maximums (x:xs) = foldl f [x] xs
  where f ys y = case fst (head ys) `compare` fst y of
                      GT -> ys
                      EQ -> y:ys
                      LT -> [y]

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (`unless` s)

putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn
