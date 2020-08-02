{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving,
             KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Fused.HTTP where

import Control.Algebra
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import Control.Carrier.Lift

newtype HttpC m a = HttpC { runHttpC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

data HTTP (m :: * -> *) k
  = Open String (m k)
  | Close (m k)
  | Post String (String -> m k)
  | HGet (String -> m k)
    deriving stock (Functor, Generic1)
    deriving anyclass (HFunctor, Effect)

open' :: String -> HttpM ()
open' s = send (Open s (pure ()))

close' :: HttpM ()
close' = send (Close (pure ()))

post' :: String -> HttpM String
post' s = send (Post s pure)

get' :: HttpM String
get' = send (HGet pure)

type HttpM = HttpC (LiftC IO)

instance (Effect sig, Algebra sig m) => Algebra (HTTP :+: sig) (HttpC m) where
  alg (L act) = case act of
    Open _ k -> k
    Close k  -> k
    Post s k -> k ("posted " <> s)
    HGet k   -> k "lmao"
  alg (R other) = HttpC (alg (handleCoercible other))

runHttp :: HttpM a -> IO a
runHttp = runM . runHttpC
