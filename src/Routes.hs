{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Routes where

import qualified Data.ByteString as BSS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import Servant

api :: Proxy.Proxy API
api = Proxy.Proxy

type API =
  "tiles"
    :> Capture "z" Int
    :> Capture "x" Int
    :> Capture "y" Text.Text
    :> Get '[OctetStream] (Headers '[Header "Content-Encoding" Text.Text] BSS.ByteString)
    :<|> "metadata"
    :> Get '[JSON] (HashMap.HashMap Text.Text Text.Text)
    :<|> Raw