{-# LANGUAGE TypeApplications #-}

module Fused.StatefulExcept where

import Control.Effect (run, PureC)
import Control.Effect.State.Strict as State
import Control.Effect.Error

type StateM = StateC Int (ErrorC String PureC)

get :: StateM Int
get = State.get @Int

put :: Int -> StateM ()
put = State.put @Int

throw :: String -> StateM a
throw = throwError

runStatefulExcept :: Int -> StateM a -> Either String (Int, a)
runStatefulExcept n x = run . runError $ State.runState n x
