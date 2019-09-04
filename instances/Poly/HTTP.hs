{-# LANGUAGE ScopedTypeVariables, DataKinds, TemplateHaskell, KindSignatures, GADTs, FlexibleContexts, TypeOperators, LambdaCase #-}

module Poly.HTTP where

import Polysemy

type HttpM = Sem '[Http, Final IO]

data Http (m :: * -> *) a where
  HOpen  :: String -> Http m ()
  HClose :: Http m ()
  HPost  :: String -> Http m String
  HGet  :: Http m String

makeSem ''Http

open' :: String -> HttpM ()
open'  = hOpen

close' :: HttpM ()
close' = hClose

post' :: String -> HttpM String
post' = hPost

get' :: HttpM String
get' = hGet

runHttp' :: Sem (Http ': r) a -> Sem r a
runHttp' = interpret $ \case
  HOpen _ -> pure ()
  HClose  -> pure ()
  HPost s -> pure ("posted " <> s)
  HGet    -> pure "contents"

runHttp :: HttpM a -> IO a
runHttp = runFinal . runHttp'
