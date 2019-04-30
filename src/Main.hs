{-# LANGUAGE FlexibleContexts, GADTs, CPP #-}

module Main where

import Gauge (bench, bgroup, nf)
import Gauge.Main (defaultMain)

import qualified Fused.Countdown as Fused
import qualified Simple.Countdown as Simple
import qualified MTL.Countdown as MTL

#define SUITE(nam,bare) [ bench "fused-effects" (nf MTL.nam 10000) \
                        , bench "mtl" (nf Fused.nam 10000) \
                        , bench "freer-simple" (nf Simple.nam 10000) \
                        , bench "manual" (nf bare 10000) ]

main :: IO ()
main = defaultMain
  [
    bgroup "Countdown"
    [ bgroup "Put" SUITE(countDownPut, bareDown)
    , bgroup "Put+Exc" SUITE(countDownExc, bareDownExc)
    ]
  ]

bareDown :: Int -> (Int, Int)
bareDown st = if st <= 0 then (st, st) else bareDown (st - 1)

bareDownExc :: Int -> Either String (Int, Int)
bareDownExc st = if st <= 0 then Left "what" else bareDownExc (st - 1)
