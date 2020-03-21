{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Routes where

import qualified Data.ByteString as BSS
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import Servant

api :: Proxy.Proxy API
api = Proxy.Proxy

type API ="tiles"
    :> Capture "z" Int
    :> Capture "x" Int
    :> Capture "y" Text.Text
    :> Get '[OctetStream] BSS.ByteString
    :<|> Raw