{-# LANGUAGE OverloadedStrings #-}

module ParagraphMakerSpec (spec) where

import Test.Hspec
import ParagraphMaker
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

page1 = T.unlines [
            "<html><body>",
            "<h1>Header</h1>",
            "<p>text and some <em>other</em> words <span class=\"class\">that I</span> have in my head now</p>",
            "<p>footer</p>",
            "</body></html>"
            ]

cdataPage = T.unlines [ 
    "<html><body>",
    "   I am inline\n\n\n\n and I am happy \n",
    "</body></html>"
    ]

linkPage = T.unlines [ 
    "<html><body>",
    "<a>I am <strong>top</strong>-inline\n\n\n\n and I am happy \n</a>",
    "<p>normal text</p>",
    "<code>\nvar i = -INFINITY;\n</code>",
    "<div>after <a>text</a> with variable <var>N</var> </div>",
    "   I am inline\n\n\n\n and I am happy \n",
    "</body></html>"
    ]


inlineTextPage = T.unlines [
    "<html><body>",
    "<div>after <div>text</div> with variable </div>",
    "</body></html>"
   ]

assertP page parNumber property expectedValue = do
  pars <- paragraphsFromText page
  property (pars !! (parNumber-1)) `shouldBe` expectedValue
  
spec :: Spec
spec = do
  describe "makeParagraphs" $ do
    context "basic" $ do
      it "returns three paragraphs" $ do
        pars <- paragraphsFromText page1
        length  pars `shouldBe` 3

      it "returns correct paragraph 1" $ do
        assertP page1 1 text "Header"
        assertP page1 1 tagCount 0

      it "returns correct paragraph 2" $ do
         assertP page1 2 text "text and some other words that I have in my head now"
         assertP page1 2 tagCount 2
    
      it "returns correct paragraph 3" $ do
        assertP page1 3 text "footer"
        assertP page1 3 tagCount 0

    context "cdata" $ do
      it "returns one paragraph" $ do
        pars <- paragraphsFromText cdataPage
        length  pars `shouldBe` 1

    context "links" $ do
      it "returns four paragraphs" $ do
        pars <- paragraphsFromText linkPage
        length  pars `shouldBe` 5

      it "returns correct paragraph 1" $ do
        assertP linkPage 1 text "I am top-inline\n and I am happy"
        assertP linkPage 1 tagCount 2
        assertP linkPage 1 wordCount 7
        assertP linkPage 1 linkCharCount 36

      it "returns correct paragraph 2" $ do
        assertP linkPage 2 text "normal text"
        assertP linkPage 2 tagCount 0
        assertP linkPage 2 wordCount 2

      it "returns correct paragraph 3" $ do
        assertP linkPage 3 text "var i = -INFINITY;"
        assertP linkPage 3 tagCount 1
        assertP linkPage 3 wordCount 4

      it "returns correct paragraph 4" $ do
        assertP linkPage 4 text "after text with variable N"
        assertP linkPage 4 tagCount 2
        assertP linkPage 4 wordCount 5
        assertP linkPage 4 linkCharCount 20

      it "returns correct paragraph 5" $ do
        assertP linkPage 5 text "I am inline\n and I am happy"
        assertP linkPage 5 tagCount 0
        assertP linkPage 5 wordCount 7

    context "inlin text" $ do
      it "returns four paragraphs" $ do
        pars <- paragraphsFromText inlineTextPage
        length  pars `shouldBe` 3

      it "returns correct paragraph 1" $ do
        assertP inlineTextPage 1 text "after"

      it "returns correct paragraph 2" $ do
        assertP inlineTextPage 2 text "text"

      it "returns correct paragraph 3" $ do
        assertP inlineTextPage 3 text "with variable"
