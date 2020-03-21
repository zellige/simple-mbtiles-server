module Controller where

import qualified DB
import qualified Database.Mbtiles as Mbtiles
import qualified Routes
import qualified Servant

server :: Mbtiles.MbtilesPool -> Servant.Server Routes.API
server spatialConns = DB.tilesDB spatialConns Servant.:<|> Servant.serveDirectoryFileServer "."
