{-# LANGUAGE TypeApplications #-}

module Fused.Stateful where

import Control.Algebra
import Data.Functor.Identity
import Control.Carrier.State.Strict as State

type StateM = StateC Int Identity

get :: StateM Int
get = State.get @Int

put :: Int -> StateM ()
put = State.put @Int

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = run $ State.runState n x
