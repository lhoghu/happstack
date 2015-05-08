{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             OverloadedStrings #-}

module Templates where

import Happstack.Server 
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Internal as TBI

data_toggle :: BH.AttributeValue  -- ^ Attribute value.
            -> BH.Attribute       -- ^ Resulting attribute.
data_toggle = TBI.attribute "data-toggle" " data-toggle=\""

data_target :: BH.AttributeValue  -- ^ Attribute value.
       -> BH.Attribute       -- ^ Resulting attribute.
data_target = TBI.attribute "data-target" " data-target=\""

appTemplate :: BH.Html -> BH.Html -> ServerPart Response
appTemplate title body = ok $ toResponse $ renderHtml $ do
    BH.head $ do
        BH.title title
        BH.meta BH.! BA.httpEquiv "Content-Type"
                BH.! BA.content "text/html;charset=utf-8"
        BH.meta BH.! BA.name "viewport"
                BH.! BA.content "width=device-width, initial-scale=1.0"

        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.href "https://maxcdn.bootstrapcdn.com/bootstrap/2.3.2/css/bootstrap.min.css"
        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.style "text/css"
                BH.! BA.href "../static/css/bootstrap-responsive.min.css"
        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.style "text/css"
                BH.! BA.href "../static/css/stdtheme.css"

        BH.script BH.! BA.src "https://maxcdn.bootstrapcdn.com/bootstrap/2.3.2/js/bootstrap.min.js" $ ""

    BH.body $ do
        BH.div BH.! BA.id "wrap" $ do

            -- Fixed navbar
            BH.div BH.! BA.class_ "navbar navbar-inverse naavbar-fixed-top" $
                BH.div BH.! BA.class_ "navbar-inner" $
                    BH.div BH.! BA.class_ "container" $ do
                        BH.button BH.! BA.type_ "button"
                                  BH.! BA.class_ "btn btn-navbar"
                                  BH.! data_toggle "collapse"
                                  BH.! data_target ".nav-collapse" $ do
                            BH.span BH.! BA.class_ "icon-bar" $ ""
                            BH.span BH.! BA.class_ "icon-bar" $ ""
                            BH.span BH.! BA.class_ "icon-bar" $ ""
                        BH.a BH.! BA.class_ "brand" 
                             BH.! BA.href "/home" $ "Intranet"
                        BH.div BH.! BA.class_ "nav-collapse collapse" $
                            BH.ul BH.! BA.class_ "nav" $ do 
                                BH.li $ BH.a BH.! BA.href "/home" $ "Home"
                                BH.li $ BH.a BH.! BA.href "/index" $ "Index"
                                BH.li BH.! BA.class_ "dropdown" $ do 
                                    BH.a BH.! BA.href "/index"
                                         BH.! BA.class_ "dropdown-toggle"
                                         BH.! data_toggle "dropdown" $ do 
                                            BH.toHtml ("Dropdown " :: String) 
                                            BH.b BH.! BA.class_ "caret" $ ""
                                    BH.ul BH.! BA.class_ "dropdown-menu" $ do 
                                        BH.li $ BH.a BH.! BA.href "/table" $
                                            "Table"
                                        BH.li $ BH.a BH.! BA.href "/form" $
                                            "Form"
                                        BH.li BH.! BA.class_ "divider" $ ""
                                        BH.li BH.! BA.class_ "nav-header" $
                                            "Guides"
                                        BH.li $ BH.a BH.! BA.href "/good" $
                                            "Good dialogue"
            -- Page content
            BH.div BH.! BA.class_ "container" $ body

        -- Footer
        BH.footer BH.! BA.id "footer" $ BH.div BH.! BA.class_ "container" $ do
            BH.p BH.! BA.class_ "muted credit" $ do 
                BH.toHtml ("Powered by " :: String)
                BH.a BH.! BA.href "http://www.happstack.com" $ "happstack"
            BH.ul BH.! BA.class_ "inline" $ do
                BH.li $ BH.a BH.! BA.href "/index" $ "Index"
                BH.li BH.! BA.class_ "muted" $ 
                    BH.preEscapedToHtml ("&middot;" :: String)
                BH.li $ BH.a BH.! BA.href "/portfolio" $ "Portfolio manager"
