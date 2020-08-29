module Controller where

import qualified DB
import qualified Database.Mbtiles as Mbtiles
import qualified Routes
import qualified Servant
import qualified Types.Config as Config

server :: Config.ContentEncoding -> Mbtiles.MbtilesPool -> Servant.Server Routes.API
server contentEncoding spatialConns = DB.tilesDB contentEncoding spatialConns Servant.:<|> DB.metadataDB spatialConns Servant.:<|> Servant.serveDirectoryFileServer "static"
