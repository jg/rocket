{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Client
import Preparer
import qualified Data.Text as T
import qualified ParagraphMaker as PM
import qualified Classifier as C

-- f :: IO ()
-- f = do
--   doc <- docAtUrl "http://blog.kaggle.com/2015/11/09/profiling-top-kagglers-gilberto-titericz-new-1-in-the-world/"
--   let filtered = filterScriptTags doc in do
--     writeFile "unfiltered.html" (T.unpack doc)
--     writeFile "filtered.html" (T.unpack filtered)



f1 = do
  doc <- docAtUrl "http://blog.kaggle.com/2015/11/09/profiling-top-kagglers-gilberto-titericz-new-1-in-the-world/"
  writeFile "filtered.html" (T.unpack (filterCdata doc))
  return ()
