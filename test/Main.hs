{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Tutorial01_Basics
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the '01 - Basics' tutorial.
-}
module Main where

import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import FRP.Rhine (Arrow (arr), ClSF, Clock (Tag), arrMCl, returnA, tagS, (&&&), (>>>), waitClock)
import FRP.Rhine.Monomer
import Monomer
import Monomer.Lens qualified as L
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import UnliftIO.STM (newTChanIO, newTQueueIO)
import Prelude
import FRP.Rhine.Monomer.Generic (GenericAppEvent (Send))

newtype AppModel = AppModel
    { _clickCount :: Int
    }
    deriving stock (Eq, Show)

data AppEvent
    = AppInit
    | AppIncrease
    deriving stock (Eq, Show)

makeLenses 'AppModel

buildUI
    :: WidgetEnv AppModel (GenericAppEvent AppModel AppEvent)
    -> AppModel
    -> WidgetNode AppModel (GenericAppEvent AppModel AppEvent)
buildUI _ model = widgetTree
  where
    widgetTree =
        vstack
            [ label "Hello world"
            , spacer
            , hstack
                [ label $ "Click count: " <> (Text.pack . show) (model ^. clickCount)
                , spacer
                , button "Increase count" (Send AppIncrease)
                ]
            ]
            `styleBasic` [padding 10]

-- handleEvent
--   :: WidgetEnv AppModel AppEvent
--   -> WidgetNode AppModel AppEvent
--   -> AppModel
--   -> AppEvent
--   -> [AppEventResponse AppModel AppEvent]
-- handleEvent wenv node model evt = case evt of
--   AppInit -> []
--   AppIncrease -> [Model (model & clickCount +~ 1)]

handleEventS :: (MonadIO m, Tag cl ~ AppEvent) => ClSF m cl AppModel AppModel
handleEventS = tagS &&& returnA >>> arr (uncurry handleEvent)

handleEvent :: AppEvent -> AppModel -> AppModel
handleEvent AppInit = clickCount .~ 7
handleEvent AppIncrease = clickCount %~ succ

main :: IO ()
main = do
    flowMonomer'
        (AppModel 0)
        handleEventS
        (waitClock @1000)
        (arr \(AppModel i) -> AppModel (succ i))
        buildUI
        [ appWindowTitle "Tutorial 01 - Basics"
        , appWindowIcon "test/assets/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "test/assets/Roboto-Regular.ttf"
        , appInitEvent (Send AppInit)
        ]
