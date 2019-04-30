{-# LANGUAGE FlexibleContexts #-}

module Bench.Countdown where

import qualified Bench.Signature.Stateful as S
import qualified Bench.Signature.StatefulExcept as SE

countDownPut :: Int -> (Int, Int)
countDownPut start = S.runStateful start go where
  go = S.get >>= (\n -> if n < 0 then pure n else S.put (n - 1) *> go)

countDownExc :: Int -> Either String (Int, Int)
countDownExc start = SE.runStatefulExcept start go where
  go = SE.get >>= (\n -> if n <= 0 then SE.throw "what" else SE.put (n - 1) *> go)
