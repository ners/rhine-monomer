{-# OPTIONS_GHC -Wno-missing-role-annotations #-}

module FRP.Rhine.Monomer where

import FRP.Rhine
    ( ClSF
    , Clock (Tag, Time)
    , MonadIO (liftIO)
    , Rhine
    , arrMCl
    , eventClockOn
    , feedbackRhine
    , flow
    , keepLast
    , (@>-^)
    , (@>>^)
    , (@@)
    , (^>>@), (|@|), UTCTime, GetClockProxy, In, Out
    )
import FRP.Rhine.Monomer.Generic (generifyConfigs, generifyUiBuilder, handleGenericEvent, GenericAppEvent (Init))
import Monomer (AppConfig (..), AppUIBuilder, EventResponse (Model), WidgetEvent, WidgetModel, startApp, appInitEvent)
import UnliftIO (MonadUnliftIO, askRunInIO)
import UnliftIO.Chan (newChan)
import Prelude
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Schedule.Class (MonadSchedule)

flowMonomer'
    :: forall m simClock model event event'
     . ( MonadUnliftIO m
       , MonadSchedule m
       , Eq model
       , WidgetModel model
       , WidgetEvent event
       , event' ~ GenericAppEvent model event
       , Time simClock ~ UTCTime
       , Clock m simClock
       , GetClockProxy simClock
       , simClock ~ In simClock
       , simClock ~ Out simClock
       )
    => model
    -> (forall m' cl. (MonadIO m', Tag cl ~ event) => ClSF m' cl model model)
    -> simClock
    -> ClSF m simClock model model
    -> AppUIBuilder model event'
    -> [AppConfig model event']
    -> m ()
flowMonomer' initialModel handleAppEvent simClock simulate uiBuilder configs = do
    chan <- newChan @_ @event
    unliftIO <- askRunInIO
    let something put = unliftIO . flow $
            feedbackRhine (keepLast initialModel) (feedbackify handleAppEventRh |@| feedbackify simulateRh) @>-^ arrMCl (liftIO . put . pure . Model)
        handleAppEventRh = handleAppEvent @@ eventClockOn @m chan
        simulateRh = simulate @@ simClock
    liftIO $
        Monomer.startApp
            initialModel
            (handleGenericEvent chan something)
            uiBuilder
            (appInitEvent Init : configs)

-- flowMonomer
--     :: forall m model event
--      . ( MonadUnliftIO m
--        , Eq model
--        , WidgetModel model
--        , WidgetEvent event
--        )
--     => model
--     -> (forall m' cl. (MonadIO m', Tag cl ~ event) => ClSF m' cl model model)
--     -> AppUIBuilder model event
--     -> [AppConfig model event]
--     -> m ()
-- flowMonomer initialModel handleAppEvent uiBuilder configs = do
--     chan <- newChan @_ @event
--     unliftIO <- askRunInIO
--     let something put = unliftIO . flow $ feedbackRhine (keepLast initialModel) (feedbackify handleAppEventRh) @>-^ arrMCl (liftIO . put . pure . Model)
--         handleAppEventRh = handleAppEvent @@ eventClockOn @m chan
--     liftIO $
--         Monomer.startApp
--             initialModel
--             (handleGenericEvent chan something)
--             (generifyUiBuilder uiBuilder)
--             (generifyConfigs configs)

feedbackify :: (Monad m) => Rhine m cl a a -> Rhine m cl ((), a) (a, a)
feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))
