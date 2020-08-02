{-# LANGUAGE TypeApplications #-}

module Fused.StatefulExcept where

import Control.Carrier.State.Strict as State
import Control.Carrier.Error.Either
import Data.Functor.Identity

type StateM = StateC Int (ErrorC String Identity)

get :: StateM Int
get = State.get @Int

put :: Int -> StateM ()
put = State.put @Int

throw :: String -> StateM a
throw = throwError

runStatefulExcept :: Int -> StateM a -> Either String (Int, a)
runStatefulExcept n x = run . runError $ State.runState n x
