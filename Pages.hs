{-# LANGUAGE OverloadedStrings #-}

module Pages (template
             , renderText
             , renderDir
             , uploadPage
             , homePage
             , donnerPage
             , donnerAddPage
             ) where

import           Types

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Data.List                   (sort)
import           Data.Monoid
import qualified Data.Text.Lazy              as T

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

template :: String -> Html -> Html
template title body = html $ do H.head $ do H.title (toHtml title)
                                            defaultIncludes
                                H.body $ do body
                                            theFooter

theFooter :: Html
theFooter = H.footer $ do
            a ! href "/files/"     $ "file serving"
            a ! href "/upload"     $ "file uploading"
            a ! href "/donnerator" $ "donnerisms"
            a ! href "/donnerfile" $ "donnerfile"
            a ! href "/donneradd"  $ "add to donnerFile"

defaultIncludes :: Html
defaultIncludes = do H.link ! A.href "/static/css/default.css" ! A.title "compact" ! A.rel "stylesheet" ! A.type_ "text/css"
                     script mempty ! src "/static/js/dropzone.js"
                     script mempty ! src "/static/js/custom-drop-form.js"

donnerPage :: String -> Html
donnerPage ism = template "Donnerator Output" $ p $ toHtml ism

donnerAddPage :: Html
donnerAddPage = template "Add to Donnerfile" $ do
  p "add line to the donnerfile"
  p "no punctuation EXCEPT A PERIOD AT THE END OF EVERY DONNERISM"
  p "capitalization is irrelevant"
  p "seriously, that period is important"
  H.form ! A.method "POST" ! action "/donneradd" $ do
    input ! type_ "text" ! name "donner_line" ! size "70"
    input ! type_ "submit" ! value "SUBMIT"
