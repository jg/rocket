{-# LANGUAGE OverloadedStrings #-}
module Classifier where

import Prelude
import ParagraphMaker
import qualified Data.Text as T
import Control.Monad.Reader
import Data.List
import Data.Monoid
import Safe

stopWords :: [T.Text]
stopWords = ["a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours", 	"ourselves", "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves"]

data ParClass = Bad | Short | Good | NearGood deriving (Show, Eq)

data Config = Config {
  _maxLinkDensity :: Float,
  _lengthLow :: Integer,
  _lengthHigh :: Integer,
  _stopWordsLow :: Float,
  _stopWordsHigh :: Float,
  _maxHeadingDistance :: Integer
}

defaultConfig :: Config
defaultConfig = Config {
  _maxLinkDensity = 0.2,
  _lengthLow = 70,
  _lengthHigh = 200,
  _stopWordsLow = 0.3,
  _stopWordsHigh = 0.32,
  _maxHeadingDistance = 200
}

data ParWithClass = ParWithClass {
  _paragraph :: Paragraph,
  _class :: ParClass
} deriving (Show, Eq)


stopWordCount :: Paragraph -> Integer
stopWordCount par = toInteger $ sum $ map (\word -> T.count word (text par)) stopWords

wordCount :: Paragraph -> Integer
wordCount = toInteger . length .  T.words . text

stopWordDensity :: Paragraph -> Float
stopWordDensity par =
  fromIntegral (stopWordCount par) / fromIntegral (wordCount par)

linkDensity :: Paragraph -> Float
linkDensity par =
  fromIntegral (linkCharCount par) / fromIntegral (T.length (text par))

parLength :: Paragraph -> Integer
parLength = toInteger . T.length . text

classify :: Paragraph -> Reader Config ParClass
classify par = do
  maxLinkDensity <- asks _maxLinkDensity
  lengthLow <- asks _lengthLow
  lengthHigh <- asks _lengthHigh
  stopWordsLow <- asks _stopWordsLow
  stopWordsHigh <- asks _stopWordsHigh

  if linkDensity par > maxLinkDensity
    then return Bad
    else if parLength par < lengthLow
         then if linkCharCount par > 0
              then return Bad
              else return Short
         else if stopWordDensity par >= stopWordsHigh
              then if parLength par > lengthHigh
                   then return Good
                   else return NearGood
              else if stopWordDensity par >= stopWordsLow
                   then return NearGood
                   else return Bad

classifyDefault :: Paragraph -> ParClass
classifyDefault par = runReader (classify par) defaultConfig

classifyParagraphDefault :: Paragraph -> ParWithClass
classifyParagraphDefault par =
  (ParWithClass par (classifyDefault par))

classifyParagraphsDefault :: [Paragraph] -> [ParWithClass]
classifyParagraphsDefault = map classifyParagraphDefault

classifyParagraph :: Paragraph -> Reader Config ParWithClass
classifyParagraph par = do
  _class <- classify par
  return $ ParWithClass par _class


classifyWithContext :: [ParWithClass] ->
                       ParWithClass ->
                       [ParWithClass] ->
                       ParClass
classifyWithContext prev current next =
  case _class current of
    Bad -> Bad
    Good -> Good
    NearGood -> let prevClass = prevGoodOrBadClass prev
                    nextClass = nextGoodOrBadClass next in
                case (prevClass, nextClass) of
                  (Just Good, _) -> Good
                  (_, Just Good) -> Good
                  _ -> Bad
    Short -> let prevClass = prevGoodOrBadClass prev
                 nextClass = nextGoodOrBadClass next in
             case (prevClass, nextClass) of
               (Just Bad, Just Bad)   -> Bad
               (Just Good, Just Good) -> Good
               (Just Bad, Just Good)  -> case prevNonShortBlock prev of
                                           Just NearGood -> Good
                                           _ -> Bad
               (Just Good, Just Bad)  -> case nextNonShortBlock next of
                                           Just NearGood -> Good
                                           _ -> Bad
               _                      -> Bad
  where
    prevGoodOrBadClass :: [ParWithClass] -> Maybe ParClass
    prevGoodOrBadClass lst =
      fmap _class (headMay $ dropWhile notGoodOrBad (reverse lst))

    nextGoodOrBadClass :: [ParWithClass] -> Maybe ParClass
    nextGoodOrBadClass lst = 
      fmap _class (headMay $ dropWhile notGoodOrBad lst)

    prevNonShortBlock :: [ParWithClass] -> Maybe ParClass
    prevNonShortBlock lst =
      fmap _class (headMay $ dropWhile nonShortBlock (reverse lst))

    nextNonShortBlock :: [ParWithClass] -> Maybe ParClass
    nextNonShortBlock lst =
      fmap _class (headMay $ dropWhile nonShortBlock lst)

    nonShortBlock :: ParWithClass -> Bool
    nonShortBlock (ParWithClass _ Short) = False
    nonShortBlock _ = True

    notGoodOrBad :: ParWithClass -> Bool
    notGoodOrBad (ParWithClass _ Good) = False
    notGoodOrBad (ParWithClass _ Bad) = False
    notGoodOrBad _ = True


split3 :: Eq a => a -> [a] -> ([a], a, [a])
split3 el lst = case elemIndex el lst of
  Just index -> let (pre, post) = splitAt index lst in (pre, el, tail post)
  Nothing -> ([], el, [])
  
-- map window function over list
-- (pre, el, post)
map3 :: Eq a => ([a] -> a -> [a] -> a) -> [a] -> [a]
map3 f lst = map invoke lst
             where
               invoke el = let (pre, x, post) = split3 el lst in f pre x post

classifyParagraphWithContext :: [ParWithClass] ->
                                ParWithClass ->
                                [ParWithClass] ->
                                ParWithClass
classifyParagraphWithContext pre el@(ParWithClass par _) post =
  ParWithClass par (classifyWithContext pre el post)

classifyParagraphsWithContext :: [ParWithClass] -> [ParWithClass]
classifyParagraphsWithContext = map3 classifyParagraphWithContext


classifyParagraphs :: [Paragraph] -> [ParWithClass]
classifyParagraphs pars =
  classifyParagraphsWithContext $ map classifyParagraphDefault pars

showParagraphHtml :: ParWithClass -> T.Text
showParagraphHtml (ParWithClass p Good) =
  "<div class='good'>" <> (text p) <> "</div>"
showParagraphHtml (ParWithClass p Bad) =
  "<div class='bad'>" <> (text p) <> "</div>"
showParagraphHtml (ParWithClass p NearGood) =
  "<div class='short'>" <> (text p) <> "</div>"
showParagraphHtml (ParWithClass p Short) =
  "<div>" <> (text p) <> "</div>"

--   go [] x xs []
--   where
--     go pre el post result = 
--     go pre el [] result = result
--   map compute lst
--   where
--     compute :: ParWithClass -> ParWithClass
--     compute el@(ParWithClass par cls) =
--       ParWithClass par (classifyWithContext (pre el lst) el (post el lst))

--     pre :: a -> [a] -> [a]
--     pre 


-- classifyParagraphs :: [Paragraph] -> Reader Config [ParWithClass]
-- classifyParagraphs pars = do
--   sequence $ map classifyParagraph pars


-- foreach (heading && short) mark all good within max distance as neargood
-- reviseGoodHeadings :: [ParWithClass] -> [ParWithClass]
-- reviseGoodHeadings pars = 
-- reviseClassification :: [ParWithClass] -> [ParWithClass]
-- reviseClassification pars = 

-- stopwordCount
-- classify :: Paragraph -> Reader Config ParClass
-- classify (Paragraph text tagCount linkCharCount) = do
