{-# LANGUAGE OverloadedStrings #-}
module ParagraphMaker where
import Prelude
import Data.Conduit
import Data.Text (Text, unpack, append)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.IO as TIO
import Text.XML.Stream.Parse
import Text.HTML.DOM
import Data.Conduit (Conduit, Producer, Consumer, (=$), ($$))
import Data.Conduit.Binary (sourceFile)
import Data.Conduit.List (sourceList)
import Data.Conduit.Combinators (sourceLazy)
import qualified Data.XML.Types as XT
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadThrow, monadThrow, runExceptionT, runResourceT)
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Identity
import Control.Monad.Trans.Resource.Internal
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as DTE
import Data.Char (isSpace)

data Paragraph = Paragraph {
  text :: Text,
  tagCount :: Integer,
  linkCharCount :: Integer,
  isHeader :: Bool
} deriving (Show, Eq)

showParagraph :: Paragraph -> Text
showParagraph p = T.concat [text p, " (tagCount: ",
                            (T.pack $ show $ tagCount p), ", ",
                            "linkC: ", (T.pack $ show $ linkCharCount p),
                            ")"]

showParagraphs :: [Paragraph] -> Text
showParagraphs lst = T.intercalate ", " (map showParagraph lst)

data ParserState = ParserState {
  -- stored text 
  textSt :: Text,
  paragraphState :: ParagraphType,
  -- number of non-block tags
  tagCountSt :: Integer,
  linkCharacterCountSt :: Integer,
  isHeadingSt :: Bool
} deriving Show

data ParagraphType = Block | Link | Br deriving Show

type WST = ResourceT (WriterT [Paragraph] (StateT ParserState IO))

headingTags = ["h1","h2","h3","h4","h5","h6"]

blockTags = ["body", -- BeginDoc event doesn't fire here, adding it to blocktags
             "blockquote","caption","center","col","colgroup","dd","div","dl",
             "dt", "fieldset","form","h1","h2","h3","h4","h5","h6","legend",
             "li", "optgroup","option","p","pre","table","td","textarea",
             "tfoot","th","thead","tr","ul "]

newParagraphState st heading = ParserState { textSt = "",
                                     paragraphState = st,
                                     tagCountSt = 0,
                                     linkCharacterCountSt = 0,
                                     isHeadingSt = heading
                                   }

fromState :: ParserState -> Paragraph
fromState st =
  Paragraph (normalizeWhitespace $ textSt st)
            (tagCountSt st) (linkCharacterCountSt st) (isHeadingSt st)

-- increase tag count by one
bumpTagCount :: ConduitM XT.Event o WST ()
bumpTagCount = do
  currentState <- lift get
  lift $ put (pureBump currentState)
  where
    pureBump :: ParserState -> ParserState
    pureBump st = let current = tagCountSt st in st { tagCountSt = current + 1 }

startNewParagraph :: ParagraphType -> Bool -> ConduitM XT.Event o WST ()
startNewParagraph parType headingBool = do
  st <- lift get
  -- put all the text collected thus far into a paragraph and push it to writer
  if ((textSt st) /= "")
    then tell [fromState st] else return ()
  lift $ put (newParagraphState parType headingBool)

normalizeWhitespace :: Text -> Text
normalizeWhitespace text = normalize text
  where
    normalize = T.strip . normalizeNewlines . normalizeSpaces
    normalizeNewlines text = normalizeChar "\n" text
    normalizeSpaces text = normalizeChar " " text
    normalizeChar c text = T.intercalate c $ filterEmpty $ T.splitOn c text
    filterEmpty = filter (/= "")


appendText :: Text -> ConduitM XT.Event o WST ()
appendText text = do
  currentState <- lift get 
  lift $ put (updateState text currentState)
  -- lift $ put (appendTextToParserState text currentState)
  where
    updateState :: Text -> ParserState -> ParserState
    updateState t st = appendTextToParserState t (maybeUpdateCharCount t st)
    appendTextToParserState :: Text -> ParserState -> ParserState
    appendTextToParserState text st = let current = textSt st in
      st { textSt = current `append` text }
    maybeUpdateCharCount :: Text -> ParserState -> ParserState
    maybeUpdateCharCount t st = let current = linkCharacterCountSt st
                                    parType = paragraphState st in
      case parType of
        Link -> st { linkCharacterCountSt = current + (toInteger (T.length t)) }
        _ -> st

maybeSetLinkType :: Text -> ConduitM XT.Event o WST ()
maybeSetLinkType tagName = do
  st <- lift get
  if tagName == "a" then lift $ put (st { paragraphState = Link }) else return ()

sink :: ConduitM XT.Event o WST ()
sink = awaitForever $ \event -> case event of 
  XT.EventBeginDocument ->
    startNewParagraph Block False
  XT.EventEndDocument ->
    startNewParagraph Block False
  XT.EventBeginElement el attrs -> do
    let tagName = XT.nameLocalName el in
      if elem tagName blockTags
      then if elem (XT.nameLocalName el) headingTags
           then (startNewParagraph Block True)
           else (startNewParagraph Block False)
      else maybeSetLinkType tagName
  XT.EventEndElement el -> do
    if elem (XT.nameLocalName el) blockTags
      then if elem (XT.nameLocalName el) headingTags
              then (startNewParagraph Block True)
              else (startNewParagraph Block False)
      else bumpTagCount
  XT.EventContent (XT.ContentText content) -> do
    if not (isBlank content) then appendText content else return ()
  XT.EventCDATA content -> do
    if not (isBlank content) then appendText content else return ()
  _ ->  return ()

isBlank :: Text -> Bool
isBlank = T.all isSpace

source = sourceFile "page.html" =$= eventConduit

-- paragraphsFromFile :: FilePath -> IO [Paragraph]
-- paragraphsFromFile path = 
--   makeParagraphs (sourceFile path =$= eventConduit $$ sink)

makeParagraphs :: Conduit () WST BS.ByteString -> IO [Paragraph]
makeParagraphs conduit =
  flip evalStateT (newParagraphState Block False) $
  execWriterT $ runResourceT (conduit =$= eventConduit $$ sink)

paragraphsFromFile path = makeParagraphs (sourceFile path)

paragraphsFromText :: Text -> IO [Paragraph]
paragraphsFromText text = makeParagraphs (sourceLazy (TLE.encodeUtf8 (TL.fromStrict text)))

run = do
  result <- flip evalStateT (newParagraphState Block False) $ execWriterT $ runResourceT $ (source $$ sink)
  TIO.putStrLn $ showParagraphs result
