{-# LANGUAGE FlexibleContexts, GADTs, CPP #-}

module Main where

import Gauge (bench, bgroup, nf, nfAppIO)
import Gauge.Main (defaultMain)
import qualified Weigh

import qualified Fused.Countdown as Fused
import qualified Simple.Countdown as Simple
import qualified Shallow.Countdown as Shallow
import qualified MTL.Countdown as MTL
import qualified Poly.Countdown as Poly
import qualified ExtEff.Countdown as ExtEff

import qualified Fused.HTTPM as Fused
import qualified Simple.HTTPM as Simple
import qualified MTL.HTTPM as MTL
import qualified Shallow.HTTPM as Shallow
import qualified Poly.HTTPM as Poly
import qualified ExtEff.HTTPM as ExtEff

#define MEMSUITE(nam,v) sequence_ [ Weigh.func "fused-effects" Fused.nam v \
                        , Weigh.func "mtl" MTL.nam v \
                        , Weigh.func "polysemy" Poly.nam v \
                        , Weigh.func "freer-simple" Simple.nam v \
                        , Weigh.func "extensible-effects" ExtEff.nam v \
                        , Weigh.func "shallow" Shallow.nam v ]

#define SUITE(nam,v) [ bench "fused-effects" (nf Fused.nam v) \
                     , bench "mtl" (nf MTL.nam v) \
                     , bench "polysemy" (nf Poly.nam v) \
                     , bench "freer-simple" (nf Simple.nam v) \
                     , bench "extensible-effects" (nf ExtEff.nam v) \
                     , bench "shallow" (nf Shallow.nam v) ]

wruns :: Int
wruns = 1

runs :: Int
runs = 10000

main :: IO ()
main = do
  putStrLn "*** Time benchmarks ***"
  defaultMain
    [
      bgroup "Countdown"
      [ bgroup "Put" SUITE(countDownPut, runs)
      , bgroup "Put+Exc" SUITE(countDownExc, runs)
      ]
    , bgroup "HTTP"
      [ bench "fused-effects" (nfAppIO Fused.doHTTP runs)
      , bench "polysemy" (nfAppIO Poly.doHTTP runs)
      , bench "extensible-effects" (nfAppIO ExtEff.doHTTP runs)
      , bench "Deep embedding" (nfAppIO MTL.doHTTP runs)
      , bench "Shallow embedding" (nfAppIO Shallow.doHTTP runs)
      , bench "freer-simple" (nfAppIO Simple.doHTTP runs)
      ]
    ]

  putStrLn "*** Space benchmarks (these will take some time to run) ***"
  Weigh.mainWith $ do
    Weigh.wgroup "Countdown" $ MEMSUITE(countDownPut, wruns)
    Weigh.wgroup "Countdown + exc" $ MEMSUITE(countDownExc, wruns)

