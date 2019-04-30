{-# LANGUAGE FlexibleContexts, GADTs, CPP #-}

module Main where

import Gauge (bench, bgroup, nf, nfAppIO)
import Gauge.Main (defaultMain)

import qualified Fused.Countdown as Fused
import qualified Simple.Countdown as Simple
import qualified Shallow.Countdown as Shallow
import qualified MTL.Countdown as MTL

import qualified Fused.HTTPM as Fused
import qualified Simple.HTTPM as Simple
import qualified MTL.HTTPM as MTL
import qualified Shallow.HTTPM as Shallow

#define SUITE(nam,v) [ bench "fused-effects" (nf Fused.nam v) \
                     , bench "mtl" (nf MTL.nam v) \
                     , bench "freer-simple" (nf Simple.nam v) \
                     , bench "shallow" (nf Shallow.nam v) ]

runs :: Int
runs = 10000

main :: IO ()
main = defaultMain
  [
    bgroup "Countdown"
    [ bgroup "Put" SUITE(countDownPut, runs)
    , bgroup "Put+Exc" SUITE(countDownExc, runs)
    ]
  , bgroup "HTTP"
    [ bench "fused-effects" (nfAppIO Fused.doHTTP runs)
    , bench "Deep embedding" (nfAppIO MTL.doHTTP runs)
    , bench "Shallow embedding" (nfAppIO Shallow.doHTTP runs)
    , bench "freer-simple" (nfAppIO Simple.doHTTP runs)
    ]
  ]

bareDown :: Int -> (Int, Int)
bareDown st = if st <= 0 then (st, st) else bareDown (st - 1)

bareDownExc :: Int -> Either String (Int, Int)
bareDownExc st = if st <= 0 then Left "what" else bareDownExc (st - 1)
