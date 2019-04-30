module Shallow.Stateful where

import Control.Monad

newtype StateM a = StateM { unStateM :: Int -> (Int, a) }

instance Functor StateM where
  fmap f (StateM a) = StateM (\n -> fmap f (a n))

instance Applicative StateM where
  pure x = StateM (\n -> (n, x))
  (<*>) = ap

instance Monad StateM where
  StateM a >>= f = StateM $ \n -> let (x, y) = a n in unStateM (f y) x

get :: StateM Int
get = StateM $ \n -> (n, n)

put :: Int -> StateM ()
put x = StateM (\_ -> (x, ()))

runStateful :: Int -> StateM a -> (Int, a)
runStateful n (StateM f) = f $! n
