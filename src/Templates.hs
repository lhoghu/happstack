{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             OverloadedStrings #-}

module Templates where

import Happstack.Server 
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Bootstrap as BB

appTemplate :: BH.Html -> BH.Html -> ServerPart Response
appTemplate title body = ok $ toResponse $ renderHtml $ do
    BH.head $ do
        BH.title title
        BH.meta BH.! BA.charset"utf-8"
        BH.meta BH.! BA.httpEquiv "X-UA-Compatible"
                BH.! BA.content "IE=edge"
        BH.meta BH.! BA.name "viewport"
                BH.! BA.content "width=device-width, initial-scale=1.0"

        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
        BH.link BH.! BA.rel "stylesheet"
                BH.! BA.style "text/css"
                BH.! BA.href "./static/css/stdtheme.css"

    BH.body $ do

        -- Fixed navbar
        BB.nav BH.! BA.class_ "navbar navbar-inverse navbar-fixed-top" $
            BH.div BH.! BA.class_ "container" $ do
                BH.div BH.! BA.class_ "navbar-header" $ do
                    BH.button BH.! BA.type_ "button"
                              BH.! BA.class_ "navbar-toggle collapsed"
                              BH.! BB.ariaExpanded "false"
                              BH.! BB.ariaControls "navbar"
                              BH.! BB.dataToggle "collapse"
                              BH.! BB.dataTarget "#navbar" $ do
                        BH.span BH.! BA.class_ "sr-only" $ 
                            "Toggle navigation"
                        BH.span BH.! BA.class_ "icon-bar" $ ""
                        BH.span BH.! BA.class_ "icon-bar" $ ""
                        BH.span BH.! BA.class_ "icon-bar" $ ""
                    BH.a BH.! BA.class_ "navbar-brand" 
                         BH.! BA.href "/home" $ "Intranet"
                BH.div BH.! BA.id "navbar"
                       BH.! BA.class_ "collapse navbar-collapse" $
                    BH.ul BH.! BA.class_ "nav navbar-nav navbar-right" $ do 
                        BH.li $ BH.a BH.! BA.href "/home" $ "Home"
                        BH.li $ BH.a BH.! BA.href "/index" $ "Index"
                        BH.li BH.! BA.class_ "dropdown" $ do 
                            BH.a BH.! BA.class_"dropdown-toggle"
                                 BH.! BB.role "button"
                                 BH.! BB.dataToggle "dropdown" 
                                 BH.! BB.ariaExpanded "false" $ do 
                                    BH.toHtml ("Dropdown " :: String) 
                                    BH.span BH.! BA.class_ "caret" $ ""
                            BH.ul BH.! BA.class_ "dropdown-menu" 
                                  BH.! BB.role "menu" $ do 
                                BH.li $ BH.a BH.! BA.href "/portfolio" $
                                    "Portfolio Manager"
                                BH.li $ BH.a BH.! BA.href "/table" $
                                    "Table"
                                BH.li $ BH.a BH.! BA.href "/form" $
                                    "Form"
                                BH.li BH.! BA.class_ "divider" $ ""
                                BH.li BH.! BA.class_ "dropdown-header" $
                                    "Guides"
                                BH.li $ BH.a BH.! BA.href "/good" $
                                    "Good dialogue"

        -- Page content
        BH.div BH.! BA.class_ "container-fluid" $ body

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

        -- Place at the end of the doc so the page loads faster
        BH.script BH.! BA.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js" $ ""
        BH.script BH.! BA.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js" $ ""

ypmContent :: BH.Html -> BH.Html
ypmContent content = 
    BH.div BH.! BA.class_ "row" $ do
        BH.div BH.! BA.class_ "col-sm-3 col-md-2 sidebar" $ do
            BH.ul BH.! BA.class_ "nav nav-sidebar" $ do
                BH.li $ BH.a BH.! BA.href "/portfolio" $ "Portfolio Manager"
                BH.li $ BH.a BH.! BA.href "/portfolio/show" $ "Portfolio"
                BH.li $ BH.a BH.! BA.href "/portfolio/divs" $ "Dividends"
                BH.li $ BH.a BH.! BA.href "/portfolio/mark" $ "Current Value"
            BH.ul BH.! BA.class_ "nav nav-sidebar" $ do
                BH.li $ BH.a BH.! BA.href "/portfolio/addd" $ "Add Dividend"
                BH.li $ BH.a BH.! BA.href "/portfolio/addt" $ "Add Transaction"
        BH.div BH.! BA.class_ "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main" $ content
