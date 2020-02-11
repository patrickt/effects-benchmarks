{-# LANGUAGE TypeApplications #-}

module Fused.Stateful where

import Control.Effect (PureC)
import Control.Effect.State.Lazy as State

type StateM = StateC Int PureC

get :: StateM Int
get = State.get @Int

put :: Int -> StateM ()
put = State.put @Int

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = run $ State.runState n x
