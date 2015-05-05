module Pages (userPageHtml) where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified  Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified Data.Text.Lazy as T

userPageHtml :: T.Text -> Html
userPageHtml text = html $ body $ p $ toHtml text
