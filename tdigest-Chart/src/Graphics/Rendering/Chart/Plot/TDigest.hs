{-# LANGUAGE TemplateHaskell #-}
module Graphics.Rendering.Chart.Plot.TDigest where

import Prelude ()
import Prelude.Compat
import Control.Lens
import Data.Colour (opaque, black)
import Data.Foldable (toList, for_)
import Data.Semigroup (Max (..))
import Data.Semigroup.Foldable (foldMap1)
import Data.TDigest
import Data.TDigest.Postprocess
import Numeric                     (showFFloat)

import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Layout
import Graphics.Rendering.Chart.State

data PlotTDigest comp = PlotTDigest
    { _plot_tdigest_data         :: TDigest comp  -- ^ Plot tdigest, i.e .data
    , _plot_tdigest_normalize    :: Bool          -- ^ Normalize histogram so total weight is 1.
    , _plot_tdigest_title        :: String        -- ^ Plot tile
    , _plot_tdigest_fill_style   :: FillStyle     -- ^ Fill style of the bins
    , _plot_tdigest_line_style   :: LineStyle     -- ^ Line style of the bin outlines
    , _plot_tdigest_quantiles    :: [Double]      -- ^ Which quantile lines to plot
    , _plot_tdigest_q_line_style :: LineStyle     -- ^ Line style of quantile lines
    , _plot_tdigest_deviations   :: Maybe Int     -- ^ Which stddev lines to plot
    , _plot_tdigest_d_line_style :: LineStyle     -- ^ Line style of deviation lines
    }

-- | Construct a bar chart with the given title and tdigest, using the next available colors
tdigestPlot :: String -> TDigest comp -> EC l (PlotTDigest comp)
tdigestPlot title td = do
    color <- takeColor
    pure $ PlotTDigest
        { _plot_tdigest_data         = td
        , _plot_tdigest_normalize    = False
        , _plot_tdigest_title        = title
        , _plot_tdigest_fill_style   = solidFillStyle color
        , _plot_tdigest_line_style   = solidLine 1.0 $ opaque black
        , _plot_tdigest_quantiles    = []
        , _plot_tdigest_q_line_style = dashedLine 1.0 [1,1] $ opaque black
        , _plot_tdigest_deviations   = Nothing
        , _plot_tdigest_d_line_style = dashedLine 1.0 [5,5] $ opaque black
        }

tdigestPlot' :: String -> TDigest comp -> EC (Layout Double Double) ()
tdigestPlot' title td = plot (fmap tdigestToPlot (tdigestPlot title td))

tdigestToPlot :: PlotTDigest comp -> Plot Double Double
tdigestToPlot ptd = Plot
    { _plot_render     = renderHistogram
    , _plot_legend     =
        [(_plot_tdigest_title ptd, renderLegend)] ++
        [(quantileTitle q, renderQuantileLegend) | q <- _plot_tdigest_quantiles ptd] ++
        maybe [] deviationLegend (_plot_tdigest_deviations ptd)
    , _plot_all_points = unzip allPoints
    }
  where
    td         = _plot_tdigest_data ptd
    hist'      = histogram td
    hist       = foldMap toList hist'
    lineStyle  = _plot_tdigest_line_style ptd
    fillStyle  = _plot_tdigest_fill_style ptd
    qLineStyle = _plot_tdigest_q_line_style ptd
    dLineStyle = _plot_tdigest_d_line_style ptd

    showFFloat' = showFFloat (Just 6)

    tw' = totalWeight td
    tw | _plot_tdigest_normalize ptd = tw'
       | otherwise                   = 1.0

    maxy = maxy' * 1.1
    maxy' = maybe 1.0 (getMax . foldMap1 (Max . f)) hist'
      where
        f (HistBin mi ma _ w _) =  w / (ma - mi) / tw

    somex = 0

    sigma = maybe 0 (sqrt . variance') hist'
    mu    = maybe 9 mean' hist'


    allPoints = ((somex, maxy) :) $ flip map hist $ \(HistBin mi ma x w _) ->
        let d = ma - mi
            y = w / d / tw
        in (x, y)

    -- Mean & Variance

    deviationLegend _ = [ (t, renderDeviationLegend) ]
      where
        t = showString "mean = "
          . showFFloat' mu
          . showString ", stddev = "
          . showFFloat' sigma
          $ ""

    renderDeviationLegend (Rect p1 p2) = withLineStyle dLineStyle $ do
        let y = (p_y p1 + p_y p2) / 2
        strokePath $ moveTo' (p_x p1) y `mappend` lineTo' (p_x p2) y

    renderDeviation pmap d = withLineStyle dLineStyle $ do
        let x = mu + fromIntegral d * sigma
        let path = moveTo (mapXY pmap (x, 0)) `mappend` lineTo (mapXY pmap (x, maxy))
        alignStrokePath path >>= strokePath

    -- Quantiles

    quantileTitle q
        = showFFloat' q
        . showString "q = "
        . showFFloat' (maybe 0 (quantile' q tw') hist')
        $ ""

    renderQuantileLegend (Rect p1 p2) = withLineStyle qLineStyle $ do
        let y = (p_y p1 + p_y p2) / 2
        strokePath $ moveTo' (p_x p1) y `mappend` lineTo' (p_x p2) y

    renderQuantile pmap q = withLineStyle qLineStyle $ do
        let x = maybe 0 (quantile' q tw') hist'
        let path = moveTo (mapXY pmap (x, 0)) `mappend` lineTo (mapXY pmap (x, maxy))
        alignStrokePath path >>= strokePath

    -- Histogram

    renderLegend r = withLineStyle lineStyle $ withFillStyle fillStyle $ do
        let path = rectPath r
        fillPath path
        strokePath path

    renderHistogram pmap = do
        withLineStyle lineStyle $ withFillStyle fillStyle $ do
            for_ hist $ \(HistBin mi ma _ w _) -> do
                let d = ma - mi
                    y = w / d / tw
                    path = rectPath $ Rect
                        (mapXY pmap (mi,0))
                        (mapXY pmap (ma,y))
                alignFillPath path >>= fillPath
                alignStrokePath path >>= strokePath

        for_ (_plot_tdigest_quantiles ptd) $ renderQuantile pmap
        for_ (_plot_tdigest_deviations ptd) $ \maxd ->
            for_ [-maxd .. maxd] $ renderDeviation pmap

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLenses ''PlotTDigest
