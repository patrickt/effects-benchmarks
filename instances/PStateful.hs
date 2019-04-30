{-# LANGUAGE TypeApplications, DataKinds #-}

module PStateful where

import Polysemy
import Polysemy.State as State

type StateM = Sem '[State Int]

get :: StateM Int
get = State.get

put :: Int -> StateM ()
put = State.put

runStateful :: Int -> StateM a -> (Int, a)
runStateful n x = run $ State.runState n x
