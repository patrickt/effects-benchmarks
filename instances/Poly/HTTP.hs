{-# LANGUAGE DataKinds, TemplateHaskell, KindSignatures, GADTs, FlexibleContexts, TypeOperators, LambdaCase #-}

module Poly.HTTP where

import Polysemy

type HttpM = Sem '[Http, Lift IO]

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
runHttp = runM . runHttp'

-- open' :: String -> HttpM ()
-- open'  = send . Open

-- close' :: HttpM ()
-- close' = send Close

-- post' :: String -> HttpM String
-- post' = send . Post

-- get' :: HttpM String
-- get' = send Get

-- runHttp' :: Eff (Http ': r) w -> Eff r w
-- runHttp' (Val x) = pure x
-- runHttp' (E u q) = case decomp u of
--   Right (Open _) -> runHttp (qApp q ())
--   Right Close    -> runHttp (qApp q ())
--   Right (Post d) -> runHttp (qApp q d)
--   Right Get      -> runHttp (qApp q "")
--   Left u'        -> E u' (tsingleton (runHttp . qApp q ))

-- runHttp :: HttpM a -> IO a
-- runHttp = runM . runHttp'
