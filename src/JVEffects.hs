{-# LANGUAGE TypeApplications, KindSignatures, TypeOperators, GADTs, FlexibleContexts, DataKinds #-}

module JVEffects (marks) where

import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Exception
import Control.Monad.Effect.State
import Gauge

oneGet :: Int -> (Int, Int)
oneGet n = run @Eff (runState n get)

countDown :: Int -> (Int,Int)
countDown start = run @Eff (runState start go)
  where go = get >>= (\n -> if n <= 0 then pure n else put (n-1) >> go)

countDownExc :: Int -> Either String (Int,Int)
countDownExc start = run @Eff $ runError (runState start go)
  where go = get >>= (\n -> if n <= (0 :: Int) then throwError "wat" else put (n-1) >> go)

data HTTP (m :: * -> *) out where
  Open :: String -> HTTP m ()
  Close :: HTTP m ()
  Post  :: String -> HTTP m String
  HGet   :: HTTP m String

instance Effect HTTP where
  handleState _ _ _ = error "Rob said we didn't need this"

instance PureEffect HTTP

open' :: Member HTTP r => String -> Eff r ()
open'  = send . Open

close' :: Member HTTP r => Eff r ()
close' = send Close

post' :: Member HTTP r => String -> Eff r String
post' = send . Post

get' :: Member HTTP r => Eff r String
get' = send HGet

runHTTP :: PureEffects e => Eff (HTTP ': e) b -> Eff e b
runHTTP = interpret
  (\x -> case x of
     Open _ -> pure ()
     Close  -> pure ()
     Post a -> pure a
     HGet   -> pure "gotten")

prog :: Member HTTP r => Eff r ()
prog = open' "cats" >> get' >> post' "cats" >> close'

p :: Member HTTP r => Int -> Eff r ()
p count   =  open' "cats" >> replicateM_ count (get' >> post' "cats") >>  close'

marks :: Benchmark
marks = bgroup "joshvera/effects"
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
