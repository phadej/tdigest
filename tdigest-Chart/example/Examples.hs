{-# LANGUAGE DataKinds #-}
module Main (main) where

import Prelude ()
import Prelude.Compat
import Control.Lens   ((&), (.~))
import Control.Monad  (replicateM)
import Data.TDigest

import Statistics.Distribution            (ContGen (..))
import Statistics.Distribution.ChiSquared (chiSquared)
import Statistics.Distribution.Normal     (standard)

import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Graphics.Rendering.Chart.Easy             as Chart
import qualified Graphics.Rendering.Chart.Plot.TDigest     as Chart
import qualified System.Random.MWC                         as MWC

example1 :: IO ()
example1 = do
    xs <- randomStream standard
    let td = tdigest xs :: TDigest 10
    Chart.toFile Chart.def "example1.svg" $ do
        Chart.layout_title Chart..= "Normal distribution"
        Chart.plot $ do
            p <- Chart.tdigestPlot "tdigest" td
            return $ Chart.tdigestToPlot $ p
                & Chart.plot_tdigest_normalize .~ True
                & Chart.plot_tdigest_deviations .~ Just 3

example2 :: IO ()
example2 = do
    xs <- randomStream $ chiSquared 5
    let td = tdigest xs :: TDigest 10
    Chart.toFile Chart.def "example2.svg" $ do
        Chart.layout_title Chart..= "Chi-squared distribution, k = 5"
        Chart.plot $ do
            p <- Chart.tdigestPlot "tdigest" td
            return $ Chart.tdigestToPlot $ p
                & Chart.plot_tdigest_normalize .~ True
                & Chart.plot_tdigest_quantiles .~ [0.5, 0.9, 0.999]

main :: IO ()
main = do
    example1
    example2

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

randomStream :: ContGen d => d -> IO [Double]
randomStream d = do
    g <- MWC.create
    replicateM 100000 (genContVar d g)
