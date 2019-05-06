{-# LANGUAGE DataKinds, TypeApplications #-}

module ExtEff.Stateful where

import Control.Eff
import Control.Eff.State.Strict as State
import Data.Tuple

type StateM = Eff '[State Int]

get :: StateM Int
get = State.get @Int

put :: Int -> StateM ()
put = State.put @Int

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = swap . run $ State.runState n x
