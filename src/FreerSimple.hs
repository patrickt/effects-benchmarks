{-# LANGUAGE TypeOperators, GADTs, FlexibleContexts, DataKinds #-}

module FreerSimple (marks) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Error
import Gauge

data HTTP a where
  Open :: String -> HTTP ()
  Close :: HTTP ()
  Post  :: String -> HTTP String
  HGet   :: HTTP String

open' :: Member HTTP r => String -> Eff r ()
open'  = send . Open

close' :: Member HTTP r => Eff r ()
close' = send Close

post' :: Member HTTP r => String -> Eff r String
post' = send . Post

get' :: Member HTTP r => Eff r String
get' = send HGet

runHTTP :: Eff (HTTP ': e) b -> Eff e b
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

oneGet :: Int -> (Int, Int)
oneGet n = run (runState n get)

countDown :: Int -> (Int,Int)
countDown start = run (runState start go)
  where go = get >>= (\n -> if n <= 0 then pure n else put (n-1) >> go)

countDownExc :: Int -> Either String (Int,Int)
countDownExc start = run $ runError (runState start go)
  where go = get >>= (\n -> if n <= (0 :: Int) then throwError "wat" else put (n-1) >> go)

-- prog :: (Monad m, Member HTTP sig, Carrier sig m) => m ()
-- prog = open' "cats" >> get' >> post' "cats" >> close'

-- p :: (Monad m, Member HTTP sig, Carrier sig m) => Int -> m ()
-- p count   =  open' "cats" >> replicateM_ count (get' >> post' "cats") >>  close'

marks :: Benchmark
marks = bgroup "freer-simple"
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
