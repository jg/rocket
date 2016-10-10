{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Client
import qualified ParagraphMaker as PM
import qualified Data.Text as T
import qualified Classifier as C
import Preparer

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let urlOpt = Nothing :: Maybe (Text)
        step1 :: Text = ""
        step2 :: Text = ""
    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((formResult, formWidget), formEnctype) <- runFormPost sampleForm
    case formResult of
      FormSuccess url -> do
        let urlOpt = Just url
        step1 <- preEscapedToMarkup <$> liftIO (step1 url)
        step2 <- preEscapedToMarkup <$> liftIO (extractArticleText url)
        defaultLayout $ do
          aDomId <- newIdent
          $(widgetFile "homepage")
      _ -> do
           let urlOpt = Nothing :: Maybe Text
               step1 = "" :: Text
               step2 = "" :: Text
           defaultLayout $ do
             aDomId <- newIdent
             $(widgetFile "homepage")

handleFormResult :: FormResult Text -> IO (Text)
handleFormResult (FormSuccess url) = extractArticleText url
handleFormResult _ = do
 return $ "Error"

sampleForm :: Form (Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $
    areq textField (withSmallInput "Page url") Nothing

-- only context-free classification
step1 :: Text -> IO Text
step1 url = do
  docText <- docAtUrl url
  pars <- PM.paragraphsFromText (filterBoringTags docText)
  let classified = C.classifyParagraphsDefault pars
      texts = map C.showParagraphHtml classified
      joined = intercalate "<br>" texts in return $ joined

-- both steps of classification, context & context free
extractArticleText :: Text -> IO Text
extractArticleText url = do
  docText <- docAtUrl url
  pars <- PM.paragraphsFromText (filterBoringTags docText)
  let classified = C.classifyParagraphs pars
      texts = map C.showParagraphHtml classified
      joined = intercalate "<br>" texts in return $ joined
