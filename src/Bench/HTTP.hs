module Bench.HTTP where

import Bench.Signature.HTTP
import Control.Monad

doHTTP :: Int -> IO Int
doHTTP n = runHttp $ do
  open' "cats"
  replicateM_ n (get' *> post' "cats")
  close'
  pure n
