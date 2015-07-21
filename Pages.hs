{-# LANGUAGE OverloadedStrings #-}

module Pages (template
             , renderText
             , renderDir
             , uploadPage
             , homePage
             , videoPage
             ) where

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


renderDir :: String -> [String] -> [String] -> Html
renderDir dir fs ds =
  let fs' = sort fs
      ds' = sort ds

      frows = foldl (>>) mempty $ fmap
              (\f -> tr . td .
                     (a ! href (toValue $ mconcat ["/files/", dir, f])) $ toHtml f) fs'
      drows = foldl (>>) mempty $ fmap
              (\d -> tr. td .
                     (a ! href (toValue $ mconcat ["/files/", dir, d, "/"])) $ toHtml d) ds'
  in html $ body $ table $ do
    tr (td ! A.style "border-bottom:1px solid black" $ "files")
    frows
    tr (td ! A.style "border-bottom:1px solid black" $ "directories")
    drows

uploadPage :: Html
uploadPage = template "upload form" $ do
  H.form ! enctype "multipart/form-data" ! A.method "POST" ! action "/uploaded" $ do
    input ! type_ "file" ! name "file_upload" ! size "40" ! A.multiple ""
    table ! A.style "width:5px" $ do
      tr $
        td $ toHtml ("Directory: "::String)
      tr $
        td $ input ! type_ "submit" ! value "upload"
  p "or use fancy thing"
  script mempty ! src "/static/js/dropzone.js"
  link  ! rel "stylesheet" ! href "/static/css/dropzone.css"
  H.form mempty ! action "/uploaded" ! class_ "dropzone" ! A.id "customDropzone"



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
            a ! href "#" $ "Donnerisms"
            toHtml ("      " :: String)
            a ! href "#" $ "Donnerfile"



defaultIncludes :: Html
defaultIncludes = do H.link ! A.href "/static/css/default.css" ! A.title "compact" ! A.rel "stylesheet" ! A.type_ "text/css"
                     script mempty ! src "/static/js/custom-drop-form.js"
