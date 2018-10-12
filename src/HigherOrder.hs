{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             TypeApplications, TypeOperators, UndecidableInstances #-}

module HigherOrder (marks) where

import Control.Effect
import Control.Monad
import Data.Coerce
import Gauge.Main

data HTTP (m :: * -> *) k
  = Open String k
  | Close k
  | Post String (String -> k)
  | HGet (String -> k)
    deriving Functor

instance HFunctor HTTP where
  hmap _ = coerce

open' :: (Member HTTP sig, Carrier sig m) => String -> m ()
open' s = send (Open s (gen ()))

close' :: (Member HTTP sig, Carrier sig m) => m ()
close' = send (Close (gen ()))

post' :: (Member HTTP sig, Carrier sig m) => String -> m String
post' s = send (Post s gen)

get' :: (Member HTTP sig, Carrier sig m) => m String
get' = send (HGet gen)

newtype HTTPC m a = HTTPC { runHTTPC :: m a }

instance (Carrier sig m) => Carrier (HTTP :+: sig) (HTTPC m) where
  gen = HTTPC . gen
  alg = algS \/ (HTTPC . alg . handlePure runHTTPC) where
    algS (Open _ k) = k
    algS (Close k)  = k
    algS (Post s k) = k s
    algS (HGet k)   = k ""

runHTTP :: (Carrier sig m) => Eff (HTTPC m) a -> m a
runHTTP = runHTTPC . interpret

oneGet :: Int -> (Int, Int)
oneGet n = run (runState n get)

countDown :: Int -> (Int,Int)
countDown start = run (runState start go)
  where go = get >>= (\n -> if n <= 0 then pure n else put (n-1) >> go)

countDownExc :: Int -> Either String (Int,Int)
countDownExc start = run $ runError (runState start go)
  where go = get >>= (\n -> if n <= (0 :: Int) then throwError "wat" else put (n-1) >> go)

prog :: (Monad m, Member HTTP sig, Carrier sig m) => m ()
prog = open' "cats" >> get' >> post' "cats" >> close'

p :: (Monad m, Member HTTP sig, Carrier sig m) => Int -> m ()
p count   =  open' "cats" >> replicateM_ count (get' >> post' "cats") >>  close'

marks :: Benchmark
marks = bgroup "higher-order-effects"
  [ bgroup "State"
    [ bench "get" $ whnf oneGet 0
    , bench "countdown" $ whnf countDown 100
    ]
  , bgroup "State + Error"
    [ bench "countdownExc" $ whnf countDownExc 10000
    ]
  , bgroup "HTTP DSL"
    [ bench "simple" $ whnf (run . runHTTP) prog
    , bench "replicated" $ whnf (run . runHTTP . p) 100000
    ]
  ]
