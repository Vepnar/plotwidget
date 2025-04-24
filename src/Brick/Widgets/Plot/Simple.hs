module Brick.Widgets.Plot.Simple (
    scatterWidget,
    scatterWidget',
    areaWidget,
    areaWidget',
    candleWidget,
) where

import Lens.Micro ((^.))
import Brick.Types 

import Brick.Widgets.Plot.Core (scatter, scatter', area, area', candle, toWidget)
import Brick.Widgets.Plot.Types

widgetTemplate :: CanvasState a -> Widget ()
widgetTemplate cs = Widget Greedy Greedy $ do
    ctx <- getContext
    render $ toWidget (ctx^.availWidthL) (ctx^.availHeightL) cs
 
scatterWidget :: [Option] -> [Point] -> Widget ()
scatterWidget opts = widgetTemplate . scatter opts

scatterWidget' :: [Point] -> Widget ()
scatterWidget' = widgetTemplate . scatter'

areaWidget :: [Option] -> [Point] -> Widget ()
areaWidget op = widgetTemplate . area op

areaWidget' :: [Point] -> Widget ()
areaWidget' = widgetTemplate . area'

candleWidget :: [Candle] -> Widget ()
candleWidget = widgetTemplate . candle