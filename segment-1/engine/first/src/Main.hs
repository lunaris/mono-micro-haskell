{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Ae
import qualified Data.Text.Lazy as T.L
import qualified GHC.Generics as G
import qualified Network.HTTP.Client as HTTP.C
import qualified Web.Scotty as W
import qualified Web.Scotty.ForwardHeaders as W.FH

main :: IO ()
main = W.scotty 6060 $ do
  W.get "/first" $ do
    settings <- W.FH.forwardingHeaders
      [ "X-Custom-Header"
      , "X-Other-Header"
      ]
      HTTP.C.defaultManagerSettings
    _ <- IO.liftIO $ do
      mgr <- HTTP.C.newManager settings
      req <- HTTP.C.parseRequest "http://localhost:7070/second"
      HTTP.C.httpLbs req mgr
    debugResponse

debugResponse :: W.ActionM ()
debugResponse = do
  params <- W.params
  hdrs <- W.headers
  let res = Response params hdrs
  IO.liftIO $ print res
  W.json res

data Response
  = Response
      { _rParams  :: [W.Param]
      , _rHeaders :: [(T.L.Text, T.L.Text)]
      }
  deriving (Eq, G.Generic, Show, Ae.ToJSON)
