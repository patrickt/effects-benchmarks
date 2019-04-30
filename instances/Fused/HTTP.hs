{-# LANGUAGE TypeOperators, FlexibleInstances, KindSignatures, MultiParamTypeClasses, DeriveFunctor, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Fused.HTTP where

import Control.Effect
import Control.Effect.Sum
import Control.Effect.Carrier
import Data.Coerce
import Control.Monad.IO.Class

newtype HttpC m a = HttpC { runHttpC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

data HTTP (m :: * -> *) k
  = Open String k
  | Close k
  | Post String (String -> k)
  | HGet (String -> k)
    deriving Functor

instance Effect HTTP where
  handle state handler (Open s k) = Open s (handler . (<$ state) $ k)
  handle state handler (Close k) = Close (handler . (<$ state) $ k)
  handle state handler (Post s k) = Post s (handler . (<$ state) . k)
  handle state handler (HGet k) = HGet (handler . (<$ state) . k)

instance HFunctor HTTP where
  hmap _ = coerce

open' :: String -> HttpM ()
open' s = send (Open s (pure ()))

close' :: HttpM ()
close' = send (Close (pure ()))

post' :: String -> HttpM String
post' s = send (Post s pure)

get' :: HttpM String
get' = send (HGet pure)

type HttpM = HttpC (LiftC IO)

instance (Effect sig, Carrier sig m) => Carrier (HTTP :+: sig) (HttpC m) where
  eff (L act) = case act of
    Open _ k -> k
    Close k  -> k
    Post s k -> k ("posted " <> s)
    HGet k    -> k "lmao"
  eff (R other) = HttpC (eff (handleCoercible other))

runHttp :: HttpM a -> IO a
runHttp = runM . runHttpC
