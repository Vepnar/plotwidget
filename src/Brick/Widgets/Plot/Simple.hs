module Brick.Widgets.Plot.Simple (
    scatterWidget,
    scatterWidget',
    areaWidget,
    areaWidget',
    candleWidget',
    scatterBrailleWidget,
    scatterBrailleWidget'
) where

import Lens.Micro ((^.))
import Brick.Types 

import Brick.Widgets.Plot.Core
import Brick.Widgets.Plot.Types

widgetTemplate :: CanvasState a -> Widget ()
widgetTemplate cs = Widget Greedy Greedy $ do
    ctx <- getContext
    render $ toWidget (ctx^.availWidthL) (ctx^.availHeightL) cs
 
scatterWidget :: [Option] -> [Point] -> Widget ()
scatterWidget opts = widgetTemplate . scatter opts

scatterWidget' :: [Point] -> Widget ()
scatterWidget' = widgetTemplate . scatter'

scatterBrailleWidget :: [Option] -> [Point] -> Widget ()
scatterBrailleWidget opts = widgetTemplate . scatterBraille opts

scatterBrailleWidget' :: [Point] -> Widget ()
scatterBrailleWidget' = widgetTemplate . scatterBraille'

areaWidget :: [Option] -> [Point] -> Widget ()
areaWidget op = widgetTemplate . area op

areaWidget' :: [Point] -> Widget ()
areaWidget' = widgetTemplate . area'

candleWidget' :: [Candle] -> Widget ()
candleWidget' = widgetTemplate . candle'