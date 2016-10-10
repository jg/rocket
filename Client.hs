module Client where

import Prelude
import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as B
import qualified Data.Text.IO as TIO
import Data.Text

html :: IO B.ByteString
html = B.readFile "./test/files/hackage-website.html"

docAtUrl :: Text -> IO Text
docAtUrl url = do
  saveUrl url
  TIO.readFile tmpFileName

tmpFileName :: String
tmpFileName = "tmp.html"

saveUrl :: Text -> IO ()
saveUrl url = do
     request <- parseUrl (unpack url)
     manager <- newManager tlsManagerSettings
     runResourceT $ do
         response <- http request manager
         responseBody response C.$$+- sinkFile tmpFileName
