{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleInstances, GeneralizedNewtypeDeriving,
             KindSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances, GADTs #-}

module Fused.HTTP where

import Control.Algebra
import Control.Monad.IO.Class
import Control.Carrier.Lift

newtype HttpC m a = HttpC { runHttpC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

data HTTP (m :: * -> *) k where
  Open :: String -> HTTP m ()
  Close :: HTTP m ()
  Post :: String -> HTTP m String
  HGet :: HTTP m String

open' :: String -> HttpM ()
open' s = send (Open s)

close' :: HttpM ()
close' = send (Close)

post' :: String -> HttpM String
post' s = send (Post s)

get' :: HttpM String
get' = send (HGet)

type HttpM = HttpC (LiftC IO)

instance (Algebra sig m) => Algebra (HTTP :+: sig) (HttpC m) where
  alg hdl sig ctx = case sig of
    L (Open _) -> pure (() <$ ctx)
    L Close -> pure (() <$ ctx)
    L (Post s) -> pure (("posted " <> s) <$ ctx)
    L (HGet) -> pure ("lmao" <$ ctx)
    R other -> HttpC (alg (runHttpC . hdl) other ctx)

runHttp :: HttpM a -> IO a
runHttp = runM . runHttpC
