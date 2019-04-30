{-# LANGUAGE GADTs, LambdaCase #-}

module MTL.HTTP where

import Control.Applicative
import Control.Monad

data HttpM a where
  Pure :: a -> HttpM a
  Lift :: IO a -> HttpM a
  Then :: HttpM a -> (a -> HttpM b) -> HttpM b
  Open :: String -> HttpM ()
  Shut :: HttpM ()
  Post :: String -> HttpM String
  HGet :: HttpM String

instance Functor HttpM where
  fmap = liftA

instance Applicative HttpM where
  pure  = Pure
  (<*>) = ap

instance Monad HttpM where
  (>>=) = Then

get' :: HttpM String
get' = HGet

post' :: String -> HttpM String
post' = Post

close' :: HttpM ()
close' = Shut

open' :: String -> HttpM ()
open' = Open

runHttp :: HttpM a -> IO a
runHttp = \case
  Pure x   -> pure x
  Lift m   -> m
  Then x f -> runHttp x >>= runHttp . f
  Open _   -> pure ()
  Shut     -> pure ()
  Post s   -> pure ("posted " <> s)
  HGet     -> pure "lmap"
