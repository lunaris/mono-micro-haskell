{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Ae
import qualified Data.Text.Lazy as T.L
import qualified GHC.Generics as G
import qualified Web.Scotty as W

main :: IO ()
main = W.scotty 7070 $ do
  W.get "/second" $ do
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
