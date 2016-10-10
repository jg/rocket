{-# LANGUAGE OverloadedStrings #-}

module PreparerSpec (spec) where

import Prelude
import Test.Hspec
import qualified Data.Text as T
import Preparer

page1 = T.unlines [
            "<html><body>",
            "<p>simple page</p>",
            "<script>with a script tag</script>",
            "</body></html>"
            ]

spec :: Spec
spec = do
  describe "filterScriptTags" $ do
    context "with page1" $ do
      it "filters out the script tags" $ do
        filterScriptTags page1 `shouldBe` "<html><body>\n<p>simple page</p>\n\n</body></html>\n"

    context "real data" $ do
      it "filters out the script tags" $ do
        contents <- fmap T.pack $ readFile "test/files/kaggle-blogpost.html"
        filterScriptTags contents `shouldBe` "\t\t\n\n"
