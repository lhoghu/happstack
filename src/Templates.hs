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

-- Bootstrap attributes
data_toggle :: BH.AttributeValue  -- ^ Attribute value.
            -> BH.Attribute       -- ^ Resulting attribute.
data_toggle = TBI.attribute "data-toggle" " data-toggle=\""

data_target :: BH.AttributeValue  -- ^ Attribute value.
            -> BH.Attribute       -- ^ Resulting attribute.
data_target = TBI.attribute "data-target" " data-target=\""

aria_expanded :: BH.AttributeValue  -- ^ Attribute value.
              -> BH.Attribute       -- ^ Resulting attribute.
aria_expanded = TBI.attribute "aria-expanded" " aria-expanded=\""

aria_controls :: BH.AttributeValue  -- ^ Attribute value.
              -> BH.Attribute       -- ^ Resulting attribute.
aria_controls = TBI.attribute "aria-controls" " aria-controls=\""

aria_haspopup :: BH.AttributeValue  -- ^ Attribute value.
              -> BH.Attribute       -- ^ Resulting attribute.
aria_haspopup = TBI.attribute "aria-haspopup" " aria-haspopup=\""

aria_labelledby :: BH.AttributeValue  -- ^ Attribute value.
                -> BH.Attribute       -- ^ Resulting attribute.
aria_labelledby = TBI.attribute "aria-labelledby" " aria-labelledby=\""

role :: BH.AttributeValue  -- ^ Attribute value.
     -> BH.Attribute       -- ^ Resulting attribute.
role = TBI.attribute "role" " role=\""

appTemplate :: BH.Html -> BH.Html -> ServerPart Response
appTemplate title body = ok $ toResponse $ renderHtml $ do
    BH.head $ do
        BH.title title
        BH.meta BH.! BA.httpEquiv "Content-Type"
                BH.! BA.content "text/html;charset=utf-8"
        BH.meta BH.! BA.name "viewport"
                BH.! BA.content "width=device-width, initial-scale=1.0"

        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.style "text/css"
                BH.! BA.href "../static/css/stdtheme.css"

        BH.script BH.! BA.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js" $ ""

    BH.body $ do

        -- Fixed navbar
        BH.div BH.! BA.class_ "navbar navbar-inverse navbar-fixed-top" $
            BH.div BH.! BA.class_ "container" $ do
                BH.div BH.! BA.class_ "navbar-header" $ do
                    BH.button BH.! BA.type_ "button"
                              BH.! BA.class_ "navbar-toggle collapsed"
                              BH.! aria_expanded "false"
                              BH.! aria_controls "navbar"
                              BH.! data_toggle "collapse"
                              BH.! data_target "#navbar" $ do
                        BH.span BH.! BA.class_ "sr-only" $ 
                            "Toggle navigation"
                        BH.span BH.! BA.class_ "icon-bar" $ ""
                        BH.span BH.! BA.class_ "icon-bar" $ ""
                        BH.span BH.! BA.class_ "icon-bar" $ ""
                    BH.a BH.! BA.class_ "navbar-brand" 
                         BH.! BA.href "/home" $ "Intranet"
                BH.div BH.! BA.id "navbar"
                       BH.! BA.class_ "collapse navbar-collapse" $
                    BH.ul BH.! BA.class_ "nav navbar-nav" $ do 
                        BH.li $ BH.a BH.! BA.href "/home" $ "Home"
                        BH.li $ BH.a BH.! BA.href "/index" $ "Index"
                        BH.li BH.! BA.class_ "dropdown" $ do 
                            BH.a BH.! BA.id "navdrop"
                                 BH.! role "button"
                                 BH.! aria_expanded "false"
                                      --BH.! aria_haspopup "true"
                                 BH.! data_toggle "dropdown" $ do 
                                BH.toHtml ("Dropdown " :: String) 
                                BH.span BH.! BA.class_ "caret" $ ""
                            BH.ul BH.! BA.class_ "dropdown-menu" 
                                  BH.! role "menu"
                                  BH.! aria_labelledby "navdrop" $ do 
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
        BH.footer BH.! BA.class_ "footer" $ 
            BH.div BH.! BA.class_ "container" $ do
                BH.p BH.! BA.class_ "text-muted" $ do 
                    BH.toHtml ("Powered by " :: String)
                    BH.a BH.! BA.href "http://www.happstack.com" $ "happstack"
                BH.ul BH.! BA.class_ "list-inline" $ do
                    BH.li $ BH.a BH.! BA.href "/index" $ "Index"
                    BH.li BH.! BA.class_ "muted" $ 
                        BH.preEscapedToHtml ("&middot;" :: String)
                    BH.li $ BH.a BH.! BA.href "/portfolio" $ "Portfolio manager"
