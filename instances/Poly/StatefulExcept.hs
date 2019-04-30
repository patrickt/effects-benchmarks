{-# LANGUAGE TypeApplications, DataKinds #-}
module Poly.StatefulExcept where

import Polysemy
import Polysemy.State as State
import Polysemy.Error as Err

type StateM = Sem '[State Int, Error String]

get :: StateM Int
get = State.get @Int

put :: Int -> StateM ()
put = State.put @Int

throw :: String -> StateM a
throw = Err.throw

runStatefulExcept :: Int -> StateM a -> Either String (Int, a)
runStatefulExcept n x = run . runError $ State.runState n x
