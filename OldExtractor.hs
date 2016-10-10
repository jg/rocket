{-# LANGUAGE OverloadedStrings #-}
module Extractor where

import Prelude
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.List (maximumBy)
import Data.Function (on)

import Text.XML
import Text.XML.Cursor

-- blockTags = ["blockquote","caption","center","col","colgroup","dd","div","dl",
--              "dt", "fieldset","form","h1","h2","h3","h4","h5","h6","legend",
--              "li", "optgroup","option","p","pre","table","td","textarea",
--              "tfoot","th","thead","tr","ul "]

data Paragraph = Paragraph
    {
      text :: T.Text
    }

blockTags = ["blockquote","caption","center","col","colgroup","dd","div","dl",
             "dt", "fieldset","form","h1","h2","h3","h4","h5","h6","legend",
             "li", "optgroup","option","p","pre","table","td","textarea",
             "tfoot","th","thead","tr","ul "]

-- splitBlockTags :: Cursor -> [T.Text] -> Axis
-- splitBlockTags cursor tags = cursor $// selector
--   where selector = checkName (isBlockTag . laxName)
--         isBlockTag :: T.Text -> Bool
--         isBlockTag tag = elem tag blockTags
--         laxName :: Name -> T.Text
--         laxName = T.toCaseFold . nameLocalName


-- tokenCount :: T.Text -> Int
-- tokenCount = T.count " " - 1

copyDocumentWithNodes :: Document -> [Node] -> Document
copyDocumentWithNodes (Document prologue (Element name attrs _) epilogue) nodes = 
                       Document prologue (Element name attrs nodes) epilogue

docTopNode :: Document -> Node
docTopNode = node . fromDocument

nodesAtDepth :: Int -> Node -> [Node]
nodesAtDepth 0 root = children root
nodesAtDepth n root =
  concat (map (nodesAtDepth (n-1)) (children root))
nodesAtDepth n _ = []

-- return the depth of a tree
maxDepth :: Node -> Int
maxDepth root = case children root of
                  [] -> 0
                  (x:xs) -> 1 + maximum (map maxDepth (x:xs))
maxDepth _ = 0

-- -- node child count
childCount :: Node -> Int
childCount = length . children

children :: Node -> [Node]
children root = map node childCursors where
    cursor :: Cursor
    cursor = fromNode root
    childCursors :: [Cursor]
    childCursors = child cursor

-- -- return node with max children
maxChildrenNode :: Node -> Node
maxChildrenNode root = go root
  where
    go :: Node -> Node
    go currentNode =
      maximumBy (compare `on` childCount) (currentNode:candidates)
      where candidates = map go (children currentNode)
    go node = node

crowdedNodeChildren :: Node -> [Node]
crowdedNodeChildren = children . maxChildrenNode

tags :: [Node] -> [T.Text]
tags nodes = catMaybes $ map tagName nodes

tagName :: Node -> Maybe T.Text
tagName (NodeElement (Element name _ _)) = Just $ nameLocalName name
tagName _ = Nothing
