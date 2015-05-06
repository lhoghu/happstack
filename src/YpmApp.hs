{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             OverloadedStrings #-}

module YpmApp where

import qualified Happstack.Server as HS
import qualified Control.Monad as CM
import Control.Monad.Trans (liftIO) 
import qualified Templates as T
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Data.YahooPortfolioManager.DbAdapter as YD
import qualified Data.YahooPortfolioManager.Types as YT

run :: HS.ServerPart HS.Response
run = CM.msum
    [ HS.dir "show"    $ showPortfolio
    , HS.dir "divs"    $ showDividends
    , HS.dir "mark"    $ markToMarket
    , ypmIndex
    ]

ypmIndex :: HS.ServerPart HS.Response
ypmIndex = T.appTemplate "Portfolio Manager" $ BH.div $ do
    BH.p $ BH.a BH.! BA.href "/portfolio/show" $ "Show the current portfolio"
    BH.p $ BH.a BH.! BA.href "/portfolio/mark" $ "Show the current prtf value"
    BH.p $ BH.a BH.! BA.href "/portfolio/divs" $ "Show the dividend history"

instance BH.ToMarkup a => BH.ToMarkup (Maybe a) where
    toMarkup Nothing = BH.toMarkup ("-" :: String)
    toMarkup (Just x) = BH.toMarkup x

data Annotations = Annotations { 
    title   :: String, 
    columns :: [String] 
} deriving Show

class TableMarkup a where
    row :: a -> BH.Html

headers :: [String] -> BH.Html
headers h = BH.tr $ CM.forM_ h (\s -> BH.th $ BH.toHtml s)

table :: TableMarkup a => [String] -> [a] -> BH.Html
table hs ps = BH.table $ headers hs >> CM.forM_ ps row

markupTable :: TableMarkup a => 
               Annotations ->
               (YD.Connection -> IO [a]) -> 
               HS.ServerPart HS.Response
markupTable s f = do
    x <- liftIO $ YD.withConnection f
    T.appTemplate (BH.toHtml $ title s) $ table (columns s) x

instance TableMarkup YT.Position where
    row p = BH.tr $ BH.td (BH.toHtml $ YT.possymbol p) >> 
                    BH.td (BH.toHtml $ YT.poscurrency p) >> 
                    BH.td (BH.toHtml $ YT.posdate p) >>
                    BH.td (BH.toHtml $ YT.posposition p) >>
                    BH.td (BH.toHtml $ YT.posstrike p)

positionAnnotations = Annotations {
    title = "Portfolio Position",
    columns = ["Symbol", "Currency", "Trade Date", "Units", "Price"]
}

showPortfolio :: HS.ServerPart HS.Response 
showPortfolio = markupTable positionAnnotations YD.fetchPositions 

instance TableMarkup YT.Dividend where
    row p = BH.tr $ BH.td (BH.toHtml $ YT.divsymbol p) >> 
                    BH.td (BH.toHtml $ YT.dividend p) >> 
                    BH.td (BH.toHtml $ YT.divdate p)

dividendAnnotations = Annotations {
    title = "Dividend history",
    columns = ["Symbol", "Dividend", "Date"]
}

showDividends :: HS.ServerPart HS.Response 
showDividends = markupTable dividendAnnotations YD.fetchDividends

instance TableMarkup YT.Portfolio where
    row p = BH.tr $ BH.td (BH.toHtml $ YT.prtfsymbol p) >> 
                    BH.td (BH.toHtml $ YT.prtfalloc p) >> 
                    BH.td (BH.toHtml $ YT.prtfprice p) >> 
                    BH.td (BH.toHtml $ YT.prtfcost p) >> 
                    BH.td (BH.toHtml $ YT.prtfcurrent p) >> 
                    BH.td (BH.toHtml $ YT.prtfchange p) >> 
                    BH.td (BH.toHtml $ YT.prtfpctchange p) >> 
                    BH.td (BH.toHtml $ YT.prtfdiv p) >> 
                    BH.td (BH.toHtml $ YT.prtfpnl p) >> 
                    BH.td (BH.toHtml $ YT.prtfpctpnl p)

markToMarketAnnotations = Annotations {
    title = "Current portfolio value",
    columns = ["Symbol", "Allocation", "Price", "Cost", "Current", "Change", "(%)", "Dividends", "PnL", "(%)"]
}

markToMarket :: HS.ServerPart HS.Response 
markToMarket = markupTable markToMarketAnnotations fetchMark 

fetchMark :: YD.Connection -> IO [YT.Portfolio]
fetchMark conn = do
    symbols <- YD.fetchSymbols conn 
    YD.populateQuotesTable conn symbols
    YD.updateFx conn
    YD.fetchPortfolio conn
