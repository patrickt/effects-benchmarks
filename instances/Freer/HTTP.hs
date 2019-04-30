{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}

module Freer.HTTP where

import Control.Monad.Freer

type HttpM = Eff '[HTTP, IO]

data HTTP a where
  Open :: String -> HTTP ()
  Close :: HTTP ()
  Post  :: String -> HTTP String
  HGet   :: HTTP String

open' :: String -> HttpM ()
open'  = send . Open

close' :: HttpM ()
close' = send Close

post' :: String -> HttpM String
post' = send . Post

get' :: HttpM String
get' = send HGet

doHttp :: Eff (HTTP ': e) b -> Eff e b
doHttp = interpret
  (\x -> case x of
     Open _ -> pure ()
     Close  -> pure ()
     Post a -> pure ("posted" <> a)
     HGet   -> pure "gotten")

runHttp :: HttpM a -> IO a
runHttp = runM . doHttp
