module Web.Scotty.ForwardHeaders
  ( forwardingAllHeaders
  , forwardingHeaders
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Data.Function as Fn
import qualified Data.List as L
import qualified Data.Text.Lazy as T.L
import qualified Data.Text.Encoding as T.E
import qualified Network.HTTP.Client as HTTP.C
import qualified Network.HTTP.Types as HTTP.Types
import qualified Web.Scotty.Trans as W

forwardingAllHeaders
  :: (Monad m,
      W.ScottyError e)
  => HTTP.C.ManagerSettings
  -> W.ActionT e m HTTP.C.ManagerSettings
forwardingAllHeaders =
  forwardingHeaders []

forwardingHeaders
  :: (Monad m,
      W.ScottyError e)
  => [T.L.Text]
  -> HTTP.C.ManagerSettings
  -> W.ActionT e m HTTP.C.ManagerSettings
forwardingHeaders names s = do
  incomingHdrs <- W.headers
  let fwdHdrs = filter (flip elem names . fst) incomingHdrs
  pure s {
    HTTP.C.managerModifyRequest = pure . mergeHeaders fwdHdrs
  }

mergeHeaders :: [(T.L.Text, T.L.Text)] -> HTTP.C.Request -> HTTP.C.Request
mergeHeaders fwdHdrs req =
  req { HTTP.C.requestHeaders = allHdrs }
  where
    fwdHdrs'  = map convertHeader fwdHdrs
    givenHdrs = HTTP.C.requestHeaders req
    allHdrs   = L.nubBy ((==) `Fn.on` fst) (givenHdrs ++ fwdHdrs')

convertHeader :: (T.L.Text, T.L.Text) -> HTTP.Types.Header
convertHeader (name, val) =
  ( CI.mk (encodeToStrictUtf8 name)
  , encodeToStrictUtf8 val
  )
  where
    encodeToStrictUtf8 = T.E.encodeUtf8 . T.L.toStrict
