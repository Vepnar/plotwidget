{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

-- Core libraries
import Control.Concurrent (forkIO, threadDelay)

-- Lens library
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

-- Brick library
import Brick 
  ( App(..)
  , customMainWithDefaultVty
  , halt
  , showFirstCursor
  )
import Brick.AttrMap (attrMap)
import Brick.BChan
import Brick.Types 

import Brick.Widgets.Plot
import Control.Monad (void, forever)

import qualified Graphics.Vty as V

data CustomEvent = Counter deriving Show

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter :: Int
       }

makeLenses ''St

wave :: Int -> CanvasState ()
wave n = area [Marker '*', Primary V.brightRed, Secondary V.red] sinWave >> area' cosWave >> return ()
    where 
        x = [start, (start +0.01) .. end]
        end = 6.28 + start
        start = nModulo *0.01
        nModulo = fromIntegral $ n `mod` 628
        sinWave = zip x $ map sin x
        cosWave = zip x $ map cos x

plotUI :: St -> Widget ()
plotUI st = Widget Greedy Greedy $ 
    getContext >>= \ctx -> 
    render $ toWidget (ctx^.availWidthL) (ctx^.availHeightL) (wave (st^.stCounter))

drawUI :: St -> [Widget ()]
drawUI st = [plotUI st]

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent _ -> stLastBrickEvent .= (Just e)
        AppEvent Counter -> do
            stCounter %= (+1)
            stLastBrickEvent .= (Just e)
        _ -> return ()

initialState :: St
initialState =
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       }

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main = do
    chan <- newBChan 10

    void $ forkIO $ forever $ writeBChan chan Counter >> threadDelay 10000

    (_, vty) <- customMainWithDefaultVty (Just chan) theApp initialState
    V.shutdown vty