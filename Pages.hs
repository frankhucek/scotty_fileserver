{-# LANGUAGE OverloadedStrings #-}

module Pages (template
             , renderText
             , renderDir
             , uploadPage
             , homePage
             , videoPage
             , donnerPage
             -- , donnerFilePage
             ) where

import           Types

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import qualified Data.Text.Lazy              as T

import           Data.Monoid

import           Data.List                   (sort, sortBy)

renderText :: T.Text -> Html
renderText text = html $ body $ p $ toHtml text


homePage :: Html
homePage = renderText "home"


renderDir :: String -> [FileEntry] -> [FileEntry] -> Html
renderDir dir fs ds =
  let fs' = sort fs
      ds' = sort ds

      frows = foldl (>>) mempty $ fmap
              (\f -> tr $ do
                  td (a ! href (toValue $ mconcat ["/files/", dir, feName f]) $ toHtml $ feName f)
                  td $ toHtml $ feLastModified f
                  td $ toHtml $ feSize f) fs'

      drows = foldl (>>) mempty $ fmap
              (\d -> tr $ do
                  td (a ! href (toValue $ mconcat ["/files/", dir, feName d, "/"]) $ toHtml $ feName d)
                  td $ toHtml $ feLastModified d
                  td $ toHtml $ feSize d) ds'

  in html $ body $ table $ do
    tr $ do
      td ! A.style "border-bottom:1px solid black" $ "files"
      td ! A.style "border-bottom:1px solid black" $ "last modified"
      td ! A.style "border-bottom:1px solid black" $ "size"
    frows
    tr $ do
      td ! A.style "border-bottom:1px solid black" $ "folders"
      td ! A.style "border-bottom:1px solid black" $ "last modified"
      td ! A.style "border-bottom:1px solid black" $ "size"
    drows


uploadPage :: Html
uploadPage = template "upload form" $ do
  link  ! rel "stylesheet" ! href "/static/css/dropzone.css"
  H.form mempty ! action "/uploaded" ! class_ "dropzone" ! A.id "customdropzone"



videoPage :: String -> Html
videoPage file = template file $
  embed ! width "320" ! height "240" ! src (toValue file) ! type_ "video/mp4" ! A.style "visibility: visible"

template :: String -> Html -> Html
template title body = html $ do H.head $ do H.title (toHtml title)
                                            defaultIncludes
                                H.body $ do body
                                            theFooter
theFooter :: Html
theFooter = H.footer $ do
            a ! href "/files/"  $ "file serving"
            toHtml ("      " :: String)
            a ! href "/upload" $ "file uploading"
            toHtml ("      " :: String)
            a ! href "/donnerator" $ "Donnerisms"
            toHtml ("      " :: String)
            a ! href "#" $ "Donnerfile"



defaultIncludes :: Html
defaultIncludes = do H.link ! A.href "/static/css/default.css" ! A.title "compact" ! A.rel "stylesheet" ! A.type_ "text/css"
                     script mempty ! src "/static/js/dropzone.js"
                     script mempty ! src "/static/js/custom-drop-form.js"

donnerPage :: String -> Html
donnerPage ism = template "Donnerator Output" $ p $ toHtml ism

-- donnerFilePage :: String -> Html
-- donnerFilePage contents = template "Donnerfile" $ do
--   h2 "the donner file"
--   form ! A.method "POST" ! action "/donnerfileupdate" $ do
--     input ! type_ "text" ! value (toValue contents) ! name "dtext"
