{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shallow.StatefulExcept where

import Control.Monad

newtype StateM a = StateM { unStateM :: Int -> Either String (Int, a) }

instance Functor StateM where
  fmap f (StateM a) = StateM (\n -> fmap (fmap f) (a n))

instance Applicative StateM where
  pure x = StateM (\n -> Right (n, x))
  (<*>) = ap

instance Monad StateM where
  (StateM a) >>= f = StateM (\n -> case a n of
                                Left s -> Left s
                                Right (n', x) -> unStateM (f x) n')

get :: StateM Int
get = StateM (\n -> Right (n, n))

put :: Int -> StateM ()
put x = StateM (\_ -> Right (x, ()))

throw :: String -> StateM a
throw s = StateM (\_ -> Left s)

runStatefulExcept :: Int -> StateM a -> Either String (Int, a)
runStatefulExcept n (StateM f) = f $! n
