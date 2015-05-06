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
    , ypmIndex
    ]

ypmIndex :: HS.ServerPart HS.Response
ypmIndex = T.appTemplate "Portfolio Manager" $ BH.div $ do
    BH.p $ BH.a BH.! BA.href "/portfolio/show" $ "Show the current portfolio"
    BH.p $ BH.a BH.! BA.href "/portfolio/divs" $ "Show the dividend history"

showPortfolio :: HS.ServerPart HS.Response 
showPortfolio = do
    p <- liftIO fetchPortfolio 
    T.appTemplate "Portfolio Position" $ positionTable p

fetchPortfolio :: IO [YT.Position]
fetchPortfolio = YD.dbFile >>= YD.connect >>= \c ->
                    YD.fetchPositions c >>= \p ->
                         YD.disconnect c >> return p

positionTableRow :: YT.Position -> BH.Html
positionTableRow p = BH.tr $ BH.td (BH.toHtml $ YT.possymbol p) >> 
                             BH.td (BH.toHtml $ YT.poscurrency p) >> 
                             BH.td (BH.toHtml $ YT.posdate p) >>
                             BH.td (BH.toHtml $ YT.posposition p) >>
                             BH.td (BH.toHtml $ YT.posstrike p)

positionTableHeader :: BH.Html
positionTableHeader = BH.tr $ BH.th "Symbol" >> 
                              BH.th "Currency" >> 
                              BH.th "Trade Date" >>
                              BH.th "Units" >>
                              BH.th "Price"

positionTable :: [YT.Position] -> BH.Html
positionTable p = BH.table $ positionTableHeader >> CM.forM_ p positionTableRow

showDividends :: HS.ServerPart HS.Response 
showDividends = do 
    p <- liftIO fetchDividends
    T.appTemplate "Dividend history" $ dividendTable p

fetchDividends :: IO [YT.Dividend]
fetchDividends = YD.dbFile >>= YD.connect >>= \c ->
                    YD.fetchDividends c >>= \d ->
                         YD.disconnect c >> return d

dividendTableRow :: YT.Dividend-> BH.Html
dividendTableRow p = BH.tr $ BH.td (BH.toHtml $ YT.divsymbol p) >> 
                             BH.td (BH.toHtml $ YT.dividend p) >> 
                             BH.td (BH.toHtml $ YT.divdate p)

dividendTableHeader :: BH.Html
dividendTableHeader = BH.tr $ BH.th "Symbol" >> 
                              BH.th "Dividend" >> 
                              BH.th "Date"

dividendTable :: [YT.Dividend] -> BH.Html
dividendTable p = BH.table $ dividendTableHeader >> CM.forM_ p dividendTableRow
