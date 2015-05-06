{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             OverloadedStrings #-}

module Templates where

import Happstack.Server 
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA

appTemplate :: BH.Html -> BH.Html -> ServerPart Response
appTemplate title body = ok $ toResponse $ renderHtml $ do
    BH.head $ do
        BH.title title
        BH.meta BH.! BA.httpEquiv "Content-Type"
                BH.! BA.content "text/html;charset=utf-8"
        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.style "text/css"
                BH.! BA.href "../static/css/stdtheme.css"
    BH.body $ do
        body
        (BH.div $ BH.a BH.! BA.href "/index" $ "Index")
