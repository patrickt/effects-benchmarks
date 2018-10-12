module MTL (marks) where

import qualified Control.Monad.State as MTL
import qualified Control.Monad.Except as MTL
import Gauge

oneGet :: Int -> (Int, Int)
oneGet = MTL.runState MTL.get

countDown :: Int -> (Int,Int)
countDown = MTL.runState go
  where go = MTL.get >>= (\n -> if n <= 0 then pure n else MTL.put (n-1) >> go)

countDownExc :: Int -> Either String (Int,Int)
countDownExc = MTL.runStateT go
  where go = MTL.get >>= (\n -> if n <= (0 :: Int) then MTL.throwError "wat" else MTL.put (n-1) >> go)

marks :: Benchmark
marks = bgroup "MTL"
  [ bgroup "State"
    [ bench "get" $ whnf oneGet 0
    , bench "countdown" $ whnf countDown 100
    ]
  , bgroup "State + Error"
    [ bench "countdownExc" $ whnf countDownExc 10000
    ]
  ]
