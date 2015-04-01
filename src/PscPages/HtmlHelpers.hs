module PscPages.HtmlHelpers where

import Data.String (fromString)

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

text :: String -> H.Html
text = H.toHtml

sp :: H.Html
sp = text " "

para :: H.AttributeValue -> H.Html -> H.Html
para className content = H.p ! A.class_ className $ content

withClass :: H.AttributeValue -> H.Html -> H.Html
withClass className content = H.span ! A.class_ className $ content

linkTo :: String -> H.Html -> H.Html
linkTo href inner = H.a ! A.href (fromString href) $ inner
