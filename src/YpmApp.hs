{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             OverloadedStrings #-}

module YpmApp where

import qualified Happstack.Server as HS
import qualified Control.Monad as CM
import Control.Monad.Trans (liftIO) 
import qualified Templates as T
import qualified YpmForms as F
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Data.YahooPortfolioManager.DbAdapter as YD
import qualified Data.YahooPortfolioManager.Types as YT
import Data.Text.Format (fixed)
import Data.Text.Lazy.Builder (toLazyText)

-- | Routing table for requests on the /portfolio path
run :: HS.ServerPart HS.Response
run = CM.msum
    [ HS.dir "show"    $ showPortfolio
    , HS.dir "divs"    $ showDividends
    , HS.dir "addd"    $ addDividend
    , HS.dir "addt"    $ addTransaction
    , HS.dir "mark"    $ markToMarket
    , ypmIndex
    ]

-- | Index navigation page for /portfolio/* urls
ypmIndex :: HS.ServerPart HS.Response
ypmIndex = T.appTemplate "Portfolio Manager" $ T.ypmContent $ do
    BH.h1 BH.! BA.class_ "page-header" $ "Portfolio Manager"
    BH.div $ do
        BH.p $ do
            BH.toHtml ("Description of the application functionality with a link to the " :: String)
            BH.a BH.! BA.href 
                "https://github.com/lhoghu/yahoo-portfolio-manager" $ "github"
            BH.toHtml (" site" :: String)
    BH.h2 BH.! BA.class_ "sub-header" $ "Navigation"
    BH.div $ do
        BH.p $ BH.a BH.! BA.href "/portfolio/show" $ "Show the current portfolio"
        BH.p $ BH.a BH.! BA.href "/portfolio/mark" $ "Show the current value"
        BH.p $ BH.a BH.! BA.href "/portfolio/divs" $ "Show the dividend history"
        BH.p $ BH.a BH.! BA.href "/portfolio/addd" $ "Add a new dividend"
        BH.p $ BH.a BH.! BA.href "/portfolio/addt" $ "Add a new transaction"

instance BH.ToMarkup a => BH.ToMarkup (Maybe a) where
    toMarkup Nothing = BH.toMarkup ("-" :: String)
    toMarkup (Just x) = BH.toMarkup x

-- | String constants to label html markup for a table representation of
-- data type a
data Annotations a = Annotations { 
    title   :: String, 
    columns :: [String] 
} deriving Show

-- | Interface for data types that can be represented in an html table
class TableMarkup a where

    -- | Set the window title and table column headers
    annotations :: Annotations a

    -- | Define how the type variable a maps onto a table row.
    -- Return the html markup for a single row
    row :: a -> BH.Html

-- | Turn a vector of strings into a set of column headers
headers :: [String] -> BH.Html
headers h = BH.thead $ BH.tr $ CM.forM_ h (\s -> BH.th $ BH.toHtml s)

table :: TableMarkup a => [String] -> [a] -> BH.Html
table hs ps = BH.table BH.! BA.class_ "table table-striped" $ 
                headers hs >> BH.tbody (CM.forM_ ps row)

-- | Create html markup of a table
-- Annotations a contains the window title and table column headers
-- The function is a db retrieval function that, given a db connection,
-- returns the table content. There should be one row per list element.
markupTable :: TableMarkup a => 
               Annotations a ->
               (YD.Connection -> IO [a]) -> 
               HS.ServerPart HS.Response
markupTable s f = do
    x <- liftIO $ YD.withConnection f
    T.appTemplate (BH.toHtml $ title s) $ T.ypmContent $ do
        BH.h1 BH.! BA.class_ "page-header" $ (BH.toHtml $ title s)
        table (columns s) x

-- | Html table for current holdings
instance TableMarkup YT.Position where
    annotations = Annotations {
        title = "Portfolio Position",
        columns = ["Symbol", "Currency", "Trade Date", "Units", "Price"]
    }

    row p = BH.tr $ BH.td (format $ YT.possymbol p) >> 
                    BH.td (format $ YT.poscurrency p) >> 
                    BH.td (format $ YT.posdate p) >>
                    BH.td (format $ YT.posposition p) >>
                    BH.td (format $ YT.posstrike p)

showPortfolio :: HS.ServerPart HS.Response 
showPortfolio = markupTable annotations YD.fetchPositions 

-- | Html table for the dividend history of the current holdings
instance TableMarkup YT.Dividend where
    annotations = Annotations {
        title = "Dividend history",
        columns = ["Symbol", "Dividend", "Date"]
    }

    row p = BH.tr $ BH.td (format $ YT.divsymbol p) >> 
                    BH.td (format $ YT.dividend p) >> 
                    BH.td (format $ YT.divdate p)

showDividends :: HS.ServerPart HS.Response 
showDividends = markupTable annotations YD.fetchDividends

data Percent = Percent (Maybe Double) deriving Show

class Format a where
    format :: a -> BH.Html

instance Format String where
    format = BH.toHtml

instance Format Double where
    format x = BH.toHtml $ toLazyText $ fixed 2 x 

instance Format a => Format (Maybe a) where
    format (Just x) = format x
    format Nothing = format ("-" :: String)

instance Format Percent where
    format (Percent (Just x)) = format $ 100.0 * x
    format (Percent Nothing) = format ("-" :: String)

-- | Html table for the current value of the current holdings
instance TableMarkup YT.Portfolio where
    annotations = Annotations {
        title = "Current portfolio value",
        columns = ["Symbol", "Allocation", "Price", "Cost", 
                   "Current", "Change", "(%)", "Dividends", "PnL", "(%)"]
    }

    row p = BH.tr $ 
        BH.td (format $ YT.prtfsymbol p) >> 
        BH.td (format $ YT.prtfalloc p) >> 
        BH.td (format $ YT.prtfprice p) >> 
        BH.td (format $ YT.prtfcost p) >> 
        BH.td (format $ YT.prtfcurrent p) >> 
        BH.td (format $ YT.prtfchange p) >> 
        BH.td (format $ Percent $ YT.prtfpctchange p) >> 
        BH.td (format $ YT.prtfdiv p) >> 
        BH.td (format $ YT.prtfpnl p) >> 
        BH.td (format $ Percent $ YT.prtfpctpnl p)
        where formatter x = toLazyText $ fixed 2 x
              handleMaybe (Just x) = x
              handleMaybe Nothing = 0.0 

addDividend = HS.decodeBody (HS.defaultBodyPolicy "/tmp" 0 10000 10000) >>
              F.formHandler (F.addDivForm "" "" "") 
                            "Add dividend" 
                            "/portfolio/addd" 
                            "addDiv"
                            add
              where
              add a = (liftIO . YD.withConnection $ (flip YD.insertDividend) a) 
                        >> showDividends

addTransaction = HS.decodeBody (HS.defaultBodyPolicy "/tmp" 0 10000 10000) >>
                 F.formHandler (F.addTransForm "" "" "" "" "") 
                               "Add transaction" 
                               "/portfolio/addt" 
                               "addTrans"
                               add
              where
              add a = (liftIO . YD.withConnection $ (flip YD.insertPosition) a) 
                        >> showPortfolio

fetchMark :: YD.Connection -> IO [YT.Portfolio]
fetchMark conn = do
    symbols <- YD.fetchSymbols conn 
    YD.populateQuotesTable conn symbols
    YD.updateFx conn
    YD.fetchPortfolio conn

markToMarket :: HS.ServerPart HS.Response 
markToMarket = markupTable annotations fetchMark 
