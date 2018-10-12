module Main where

import qualified HigherOrder as HigherOrder
import qualified JVEffects as JVEffects
import qualified FreerSimple as FreerSimple
import qualified MTL as MTL
import Gauge.Main

main :: IO ()
main = defaultMain [ HigherOrder.marks
                   , JVEffects.marks
                   , FreerSimple.marks
                   , MTL.marks
                   ]
