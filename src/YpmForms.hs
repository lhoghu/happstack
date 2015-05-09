{-# LANGUAGE 
        OverloadedStrings, 
        FlexibleInstances, 
        FlexibleContexts, 
        TypeFamilies #-}

module YpmForms where

import Control.Applicative.Indexed
import Text.Reform
import Text.Regex
import Data.Text.Lazy
import Text.Blaze.Html5 hiding (label)
import Text.Blaze.Html5.Attributes hiding (label, form)
import Text.Reform.Blaze.String (label, errorList, inputText)
import qualified Data.YahooPortfolioManager.Yahoo as Y
import qualified Data.YahooPortfolioManager.DbAdapter as YD
import qualified Data.YahooPortfolioManager.Types as YT
import Control.Monad.Trans (liftIO, MonadIO) 
import Control.Monad (msum)
import Text.Reform.Happstack (environment)
import qualified Happstack.Server as H
import qualified Templates as T

data YpmFormError input = CommonError (CommonFormError input)
                        | InvalidDate
                        | InvalidSymbol
                        | InvalidPrice
                        | InvalidDividend
                        | InvalidCurrency
                        | InvalidPosition
                          deriving (Show)

instance ToMarkup (YpmFormError [H.Input]) where
    toMarkup InvalidDate = "Date should be in form yyyy-mm-dd"
    toMarkup InvalidSymbol = "Symbol not recognised by Yahoo"
    toMarkup InvalidPrice = "Price could not be converted to a number"
    toMarkup InvalidPosition = "Position could not be converted to a number"
    toMarkup InvalidDividend = "Dividend could not be converted to a number"
    toMarkup InvalidCurrency = "Currency should be a 3-letter string (e.g. EUR)"
    toMarkup (CommonError (InputMissing fid)) = 
        toMarkup $ "Internal Error. Input missing: " ++ show fid
    toMarkup (CommonError (NoStringFound input)) = 
        toMarkup $ "Internal Error. Could not extract a String from: " ++ 
                   show input
    toMarkup (CommonError (MultiStringsFound input)) = 
        toMarkup $ "Internal Error. Found more than one String in: " ++ 
                   show input

instance FormError (YpmFormError input) where
    type ErrorInputType (YpmFormError input) = input
    commonFormError = CommonError

checkRegex :: Regex -> error -> String -> Either error String
checkRegex r e s = case matchRegex r s of 
                    Just _ -> Right s
                    Nothing -> Left e

data Date = Date String deriving (Eq, Ord, Read, Show)
data ValidDate = ValidDate

validDateProof :: (Monad m) => error -> Proof m error ValidDate String String
validDateProof e = 
    Proof ValidDate 
          (return . checkRegex (mkRegex "^[0-9]{4}-[0-9]{2}-[0-9]{2}$") e)

dateForm :: (Monad m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
            String -> -- initial value that appears in the text box
            Form m input (YpmFormError input) Html ValidDate Date
dateForm i = errorList ++> 
             label ("Date" :: String) ++> 
             mapView (injectClass "form-control") (Date <<$>> 
                prove (inputText i) (validDateProof InvalidDate))

data Currency = Currency String deriving (Eq, Ord, Read, Show)
data ValidCurrency = ValidCurrency

validCcyProof :: (Monad m) => error -> Proof m error ValidCurrency String String
validCcyProof e = 
    Proof ValidCurrency (return . checkRegex (mkRegex "^[A-Za-z]{3}$") e)

ccyForm :: (Monad m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
            String -> 
            Form m input (YpmFormError input) Html ValidCurrency Currency
ccyForm i = errorList ++> 
            label ("Currency" :: String) ++> 
            mapView (injectClass "form-control") (Currency <<$>> 
                prove (inputText i) (validCcyProof InvalidCurrency))

data Position = Position String deriving (Eq, Ord, Read, Show)
data ValidPosition = ValidPosition

validPosProof :: (Monad m) => error -> Proof m error ValidPosition String String
validPosProof e = 
    Proof ValidPosition (return . checkRegex (mkRegex "^[0-9]*\\.?[0-9]*$") e)

positionForm :: (Monad m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
                String -> 
                Form m input (YpmFormError input) Html ValidPosition Position
positionForm i = errorList ++> 
                 label ("Position" :: String) ++> 
                 mapView (injectClass "form-control") (Position <<$>> 
                    prove (inputText i) (validPosProof InvalidPosition))

data Price = Price String deriving (Eq, Ord, Read, Show)
data ValidPrice = ValidPrice

validPriceProof :: (Monad m) => error -> Proof m error ValidPrice String String
validPriceProof e = 
    Proof ValidPrice (return . checkRegex (mkRegex "^[0-9]*\\.?[0-9]*$") e)

priceForm :: (Monad m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
             String -> 
             Form m input (YpmFormError input) Html ValidPrice Price
priceForm i = errorList ++> 
              label ("Price" :: String) ++> 
              mapView (injectClass "form-control") (Price <<$>> 
                    prove (inputText i) (validPriceProof InvalidPrice))

data Dividend = Dividend String deriving (Eq, Ord, Read, Show)
data ValidDividend = ValidDividend

validDivProof :: (Monad m) => error -> Proof m error ValidDividend String String
validDivProof e = 
    Proof ValidDividend (return . checkRegex (mkRegex "^[0-9]*\\.?[0-9]*$") e)

dividendForm :: (Monad m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
                String -> 
                Form m input (YpmFormError input) Html ValidDividend Dividend
dividendForm i = errorList ++> 
                 label ("Dividend" :: String) ++> 
                 mapView (injectClass "form-control") (Dividend <<$>> 
                    prove (inputText i) (validDivProof InvalidDividend))

data Symbol = Symbol String deriving (Eq, Ord, Read, Show)
data ValidSymbol = ValidSymbol

validSymbolProof :: (Monad m, MonadIO m) => error -> Proof m error ValidSymbol String String
validSymbolProof e = Proof ValidSymbol (check) 
    where
    check str = do --return $ Right str
        r <- liftIO $ Y.validateSymbol str
        if r then (return $ Right str) else (return $ Left e)

symbolForm :: (Monad m, MonadIO m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
              String -> -- initial value that appears in the text box
              Form m input (YpmFormError input) Html ValidSymbol Symbol
symbolForm i = errorList ++> 
               label ("Symbol" :: String) ++> 
               mapView (injectClass "form-control") (Symbol <<$>> 
                    prove (inputText i) (validSymbolProof InvalidSymbol))

formGroup :: Html -> Html 
formGroup v = Text.Blaze.Html5.div ! class_ "form-group" $ v

-- Inject class attribute into markup element
injectClass :: AttributeValue -> Html -> Html 
injectClass s v = v ! class_ s 

blazeForm :: Html -> AttributeValue -> Html
blazeForm html uri = form ! action uri
                          ! method "POST"
                          ! enctype "multipart/form-data" $ 
                                html >> input ! type_ "submit"

formHandler :: (ToMarkup error) => 
               Form (H.ServerPartT IO) [H.Input] error Html proof a -> 
               String ->
               AttributeValue ->
               Text ->
               (a -> H.ServerPart H.Response) ->
               H.ServerPart H.Response 
formHandler form title uri id successResponse = msum 
    [ do H.method H.GET 
         html <- viewForm id form 
         T.appTemplate (toHtml title) $ T.ypmContent $ blazeForm html uri
    , do H.method H.POST 
         r <- eitherForm environment id form 
         case r of 
            (Right a) -> successResponse a
            (Left view) -> T.appTemplate (toHtml title) $ T.ypmContent $ blazeForm view uri
    ]

data ValidDiv = ValidDiv
mkDividend :: (Monad m, Monoid view) => 
              Form m input error view 
                (ValidSymbol -> ValidDividend -> ValidDate -> ValidDiv) 
                (Symbol -> Dividend -> Date -> YT.Dividend)
mkDividend = 
    ipure (\ValidSymbol ValidDividend ValidDate -> ValidDiv) mk
    where
    mk (Symbol sym) (Dividend div) (Date dte) = 
        YT.Dividend sym (read div :: Double) dte

addDivForm :: (Monad m, MonadIO m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
              String -> -- ^ initial symbol
              String -> -- ^ initial dividend
              String -> -- ^ initial date
              Form m input (YpmFormError input) Html ValidDiv YT.Dividend
addDivForm sym div dte = mkDividend <<*>> (mapView formGroup (symbolForm sym))
                                    <<*>> (mapView formGroup (dividendForm div))
                                    <<*>> (mapView formGroup (dateForm dte))

data ValidTrans = ValidTrans
mkTransaction :: (Monad m, Monoid view) => 
                 Form m input error view 
                 (ValidSymbol -> ValidCurrency -> ValidDate -> 
                    ValidPosition -> ValidPrice -> ValidTrans) 
                 (Symbol -> Currency -> Date -> Position -> Price -> YT.Position)
mkTransaction = 
    ipure (\ValidSymbol ValidCurrency ValidDate ValidPosition ValidPrice 
                -> ValidTrans) 
          mk
    where
    mk (Symbol sym) (Currency ccy) (Date dte) (Position pos) (Price pri) = 
        YT.Position sym ccy dte (read pos :: Double) (read pri :: Double) 

addTransForm :: (Monad m, MonadIO m, Functor m, FormInput input, ToMarkup (YpmFormError input)) => 
                String -> String -> String -> String -> String ->
                Form m input (YpmFormError input) Html ValidTrans YT.Position
addTransForm sym ccy dte pos pri = 
    mkTransaction <<*>> (mapView formGroup (symbolForm sym))
                  <<*>> (mapView formGroup (ccyForm ccy))
                  <<*>> (mapView formGroup (dateForm dte))
                  <<*>> (mapView formGroup (positionForm pos))
                  <<*>> (mapView formGroup (priceForm pri))
