{-# LANGUAGE OverloadedStrings #-}

module ClassifierSpec (spec) where

import Test.Hspec
import ParagraphMaker
import Classifier
import Prelude
import qualified Data.ByteString as B
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.List (maximumBy)
import Data.Function (on)
import Text.XML
import Text.XML.Cursor
import qualified Data.Map.Lazy as M
import qualified Data.ByteString.Lazy as BL
import Classifier

-- assertClassifyParagraph
assertCP par cls = do
  classifyDefault par `shouldBe` cls

parText = "01234567890123456789" 

parWithCharCount count = (Paragraph parText 0 count False)

parWithLinkCharCount count = (Paragraph parText count 0 False)

spec :: Spec
spec = do
  describe "test max link density" $ do
    it "classifies correctly" $ do
      assertCP (parWithCharCount 0) Short
      assertCP (parWithCharCount 20) Bad
      assertCP (parWithCharCount 40) Bad
      assertCP (parWithCharCount 39) Bad
      assertCP (parWithCharCount 41) Bad

  

