{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server 
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as BH
import qualified Text.Blaze.Html4.Strict.Attributes as BA
import qualified Control.Monad as CM

main :: IO ()
main = simpleHTTP nullConf $ CM.msum 
        [ dir "home"    $ homePage 
        , dir "index"   $ indexPage 
        , dir "good"    $ goodPage 
        , seeOther ("/index" :: String) 
                   (toResponse ("Page not found. Redirecting to /index\n" :: String))
        ]

appTemplate :: BH.Html -> BH.Html -> ServerPart Response
appTemplate title body = ok $ toResponse $ BH.docTypeHtml $ do
            BH.head $ do
                BH.title title
                BH.meta ! BA.httpEquiv "Content-Type"
                        ! BA.content "text/html;charset=utf-8"
            BH.body $ do
                body
                
homePage :: ServerPart Response
homePage = appTemplate "Home page" (BH.p "Hello, from Happstack\n")

goodPage :: ServerPart Response
goodPage = appTemplate "The emporer's secret to good dialogue" 
                       (do
                        BH.p "Something, something, something dark side"
                        BH.p "Something, something, something complete")

indexPage :: ServerPart Response
indexPage = appTemplate "Index" 
                        (BH.div $ do
                           BH.p $ BH.a ! BA.href "/home" $ "Home."
                           BH.p $ BH.a ! BA.href "/good" $ "Star wars secret")
