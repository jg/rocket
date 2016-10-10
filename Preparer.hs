module Preparer where

import Prelude
import qualified Data.Text as T
import qualified Text.RegexPR as TR
-- import qualified Text.Regex.PCRE.ByteString.Utils as PR

filterPattern pattern html = T.pack $ pattern "" (T.unpack html)

-- filterScriptTags :: T.Text -> T.Text
-- filterScriptTags = filterPattern "<script[^>]*>[^<]*</script>"
-- <(?:"[^"]*"['"]*|'[^']*'['"]*|[^'">])+>

filterTagRegex :: String -> String
filterTagRegex tagName = "<" ++ tagName ++ "[^>]*>[^<]*</" ++ tagName ++">"

filterTag :: T.Text -> T.Text -> T.Text
filterTag tagName html =
  T.pack $ TR.gsubRegexPR (filterTagRegex (T.unpack tagName)) "" (T.unpack html)

-- filterTags :: [T.Text] -> T.Text -> T.Text
-- filterTags tags html = foldl filterTag html tags

filterBoringTags html = filterTag "script" (filterTag "style" html)
-- filterBoringTags = (filterTag "style") . (filterTag "script")
-- filterBoringTags = filterTags ["script", "style"]

-- ["script, "style"]
  --foldl filterTag html tags
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b

