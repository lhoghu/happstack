{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             OverloadedStrings #-}

module Main where

import Happstack.Server 
import Text.Blaze ((!))
import qualified Control.Monad as CM
import qualified Control.Applicative.Indexed as CAI
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Char8 as C
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Reform as TR
import qualified Text.Reform.Blaze.String as TRBS
import Text.Reform.Happstack (environment)
import SharedForm

main :: IO ()
main = simpleHTTP nullConf $ CM.msum 
        [ dir "home"    $ homePage 
        , dir "index"   $ indexPage 
        , dir "good"    $ goodPage 
        , dir "params"  $ paramsPage 
        , dir "form"    $ formPage
        , seeOther ("/index" :: String) 
                   (toResponse ("Page not found. Redirecting to /index\n" :: String))
        ]
-- main = 
--     do let form = userForm "" ""
--        simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
--                                formHandler form

appTemplate :: BH.Html -> BH.Html -> ServerPart Response
appTemplate title body = ok $ toResponse $ BH.docTypeHtml $ do
            BH.head $ do
                BH.title title
                BH.meta ! BA.httpEquiv "Content-Type"
                        ! BA.content "text/html;charset=utf-8"
            BH.body $ do
                body
                (BH.div $ BH.a ! BA.href "/index" $ "Index")
                
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
                           BH.p $ BH.a ! BA.href "/form" $ "Play with a form"
                           BH.p $ BH.a ! BA.href "/good" $ "Star wars secret")

paramsPage :: ServerPart Response
paramsPage = 
    look "str" >>= \s ->
    appTemplate "Passing parameters in the url" (BH.div $ showParams s)
    where showParams :: String -> BH.Html
          showParams s = BH.toHtml $ "str: " ++ s

formPage :: ServerPart Response
formPage = do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
              formHandler (userForm "" "")

instance BH.ToMarkup (DemoFormError [Input]) where
    toMarkup InvalidEmail = "Email address must contain a @."
    toMarkup InvalidUsername = "Username must not be blank."
    toMarkup (CommonError (TR.InputMissing fid)) = 
        BH.toMarkup $ "Internal Error. Input missing: " ++ show fid
    toMarkup (CommonError (TR.NoStringFound input)) = 
        BH.toMarkup $ "Internal Error. Could not extract a String from: " ++ 
                   show input
    toMarkup (CommonError (TR.MultiStringsFound input)) = 
        BH.toMarkup $ "Internal Error. Found more than one String in: " ++ 
                   show input

usernameForm :: (Monad m, 
                 TR.FormInput input, 
                 BH.ToMarkup (DemoFormError input)) =>
                String -> 
                TR.Form m input (DemoFormError input) BH.Html TR.NotNull Username
usernameForm initialValue = TRBS.errorList TR.++> 
                            (TRBS.label ("username: " :: String) TR.++> 
                            (Username CAI.<<$>> 
                                TR.prove (TRBS.inputText initialValue)
                                         (TR.notNullProof InvalidUsername)))

emailForm :: (Monad m, 
              TR.FormInput input, 
              BH.ToMarkup (DemoFormError input)) => 
             String -> 
             TR.Form m input (DemoFormError input) BH.Html ValidEmail Email
emailForm initialValue = TRBS.errorList TR.++> 
                         (TRBS.label ("email: " :: String) TR.++> 
                            (Email CAI.<<$>> 
                                TR.prove (TRBS.inputText initialValue)
                                         (validEmailProof InvalidEmail)))

userForm :: (Monad m, TR.FormInput input, BH.ToMarkup (DemoFormError input)) => 
            String -> -- ^ initial username
            String -> -- ^ initial email
            TR.Form m input (DemoFormError input) BH.Html ValidUser User
userForm nm eml = mkUser CAI.<<*>> (usernameForm nm) CAI.<<*>> (emailForm eml)

blazeForm :: BH.Html -> BH.Html
blazeForm html = BH.form ! BA.action "/form"
                         ! BA.method "POST"
                         ! BA.enctype "multipart/form-data" $
                           do html 
                              BH.input ! BA.type_ "submit"

formHandler :: (BH.ToMarkup error, Show a) => 
               TR.Form (ServerPartT IO) [Input] error BH.Html proof a -> 
               ServerPart Response 
formHandler form = CM.msum 
    [ do method GET 
         html <- TR.viewForm "user" form 
         appTemplate "Sample Form" $ blazeForm html 
    , do method POST 
         r <- TR.eitherForm environment "user" form 
         case r of 
            (Right a) -> appTemplate "Form result" $ BH.toHtml $ show a 
            (Left view) -> appTemplate "Form result" $ blazeForm view 
    ]
