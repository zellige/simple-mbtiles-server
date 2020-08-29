module Lib
  ( startApp,
  )
where

import qualified Database.Mbtiles as Mbtiles
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai (run)
import qualified Servant
import qualified Web.Browser as Browser
import qualified System.Exit as SystemExit
import qualified Data.Text as Text
import qualified Routes
import qualified Controller
import qualified DB (getTileEncoding)

debug :: Wai.Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (Wai.requestHeaders req)
  app req resp

getTilesPool :: FilePath -> IO (Either Mbtiles.MBTilesError Mbtiles.MbtilesPool)
getTilesPool file = do
  Mbtiles.getMbtilesPool file

startApp :: FilePath -> Bool -> IO ()
startApp mbtilesfile startBrowser = do
  mbTilesConnsOrError <- getTilesPool mbtilesfile
  case mbTilesConnsOrError of
    Left e -> do
      putStrLn $ show e
      SystemExit.exitWith (SystemExit.ExitFailure 2)
    Right spatialConns -> do
        contentEncoding <- DB.getTileEncoding spatialConns
        let
          action = Wai.run 8765 $ debug $ Servant.serve Routes.api (Controller.server contentEncoding spatialConns)
        if startBrowser then do
            b <- Browser.openBrowser $ Text.unpack "http://127.0.0.1" ++ ":" ++ "8765"
            if b
                then action
                else print "Failed to start browser"
        else
            action
