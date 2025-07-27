{-# OPTIONS_GHC -Wno-missing-role-annotations #-}

module FRP.Rhine.Monomer.Generic where

import Monomer
import UnliftIO.Chan (Chan, writeChan)
import Data.Typeable (Typeable)
import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Data.Functor (($>))
import Debug.Trace (trace)

data GenericAppEvent model event
    = Init
    | Send event
    | Receive [AppEventResponse model (GenericAppEvent model event)]

handleGenericEvent
    :: ( event' ~ GenericAppEvent model event
       )
    => Chan event
    -> (([AppEventResponse model event'] -> IO ()) -> IO ())
    -> WidgetEnv model event'
    -> WidgetNode model event'
    -> model
    -> event'
    -> [AppEventResponse model event']
handleGenericEvent _ flow _ _ _ Init = pure . Monomer.Producer $ flow . (. Receive)
handleGenericEvent chan _ _ _ _ (Send evt) = pure . Monomer.Task $ writeChan chan evt $> Receive [NoOpResponse]
handleGenericEvent _ _ _ _ _ (Receive responses) = responses

generifyUiBuilder :: (Typeable model, Typeable event) => AppUIBuilder model event -> AppUIBuilder model (GenericAppEvent model event)
generifyUiBuilder builder env = generifyNode . builder (ungenerifyEnv env)

generifyNode :: (Typeable model, Typeable event) => WidgetNode model event -> WidgetNode model (GenericAppEvent model event)
generifyNode WidgetNode{..} = WidgetNode
    { _wnChildren = generifyNode <$> _wnChildren
    , _wnWidget = generifyWidget _wnWidget
    , ..
    }

generifyWidget :: (Typeable model, Typeable event) => Widget model event -> Widget model (GenericAppEvent model event)
generifyWidget Widget{..} =
    Widget
        { widgetInit = \env node -> generifyWidgetResult $ widgetInit (ungenerifyEnv env) (ungenerifyNode node)
        , widgetMerge = \env node node' -> generifyWidgetResult $ widgetMerge (ungenerifyEnv env) (ungenerifyNode node) (ungenerifyNode node')
        , widgetDispose = \env node -> generifyWidgetResult $ widgetDispose (ungenerifyEnv env) (ungenerifyNode node)
        , widgetGetState = \env node -> widgetGetState (ungenerifyEnv env) (ungenerifyNode node)
        , widgetGetInstanceTree = \env node -> widgetGetInstanceTree (ungenerifyEnv env) (ungenerifyNode node)
        , widgetFindNextFocus = \env node -> widgetFindNextFocus (ungenerifyEnv env) (ungenerifyNode node)
        , widgetFindByPoint = \env node -> widgetFindByPoint (ungenerifyEnv env) (ungenerifyNode node)
        , widgetFindBranchByPath = \env node -> widgetFindBranchByPath (ungenerifyEnv env) (ungenerifyNode node)
        , widgetHandleEvent = \env node path systemEvent -> generifyWidgetResult <$> widgetHandleEvent (ungenerifyEnv env) (ungenerifyNode node) path systemEvent
        , widgetHandleMessage = \env node path i -> generifyWidgetResult <$> widgetHandleMessage (ungenerifyEnv env) (ungenerifyNode node) path i
        , widgetGetSizeReq = \env node -> widgetGetSizeReq (ungenerifyEnv env) (ungenerifyNode node)
        , widgetResize = \env node a b -> generifyWidgetResult $ widgetResize (ungenerifyEnv env) (ungenerifyNode node) a b
        , widgetRender = \env node -> widgetRender (ungenerifyEnv env) (ungenerifyNode node)
        }

ungenerifyWidget :: (Typeable model, Typeable event) => Widget model (GenericAppEvent model event) -> Widget model event
ungenerifyWidget Widget{..} =
    Widget
        { widgetInit = \env node -> ungenerifyWidgetResult $ widgetInit (generifyEnv env) (generifyNode node)
        , widgetMerge = \env node node' -> ungenerifyWidgetResult $ widgetMerge (generifyEnv env) (generifyNode node) (generifyNode node')
        , widgetDispose = \env node -> ungenerifyWidgetResult $ widgetDispose (generifyEnv env) (generifyNode node)
        , widgetGetState = \env node -> widgetGetState (generifyEnv env) (generifyNode node)
        , widgetGetInstanceTree = \env node -> widgetGetInstanceTree (generifyEnv env) (generifyNode node)
        , widgetFindNextFocus = \env node -> widgetFindNextFocus (generifyEnv env) (generifyNode node)
        , widgetFindByPoint = \env node -> widgetFindByPoint (generifyEnv env) (generifyNode node)
        , widgetFindBranchByPath = \env node -> widgetFindBranchByPath (generifyEnv env) (generifyNode node)
        , widgetHandleEvent = \env node path systemEvent -> ungenerifyWidgetResult <$> widgetHandleEvent (generifyEnv env) (generifyNode node) path systemEvent
        , widgetHandleMessage = \env node path i -> ungenerifyWidgetResult <$> widgetHandleMessage (generifyEnv env) (generifyNode node) path i
        , widgetGetSizeReq = \env node -> widgetGetSizeReq (generifyEnv env) (generifyNode node)
        , widgetResize = \env node a b -> ungenerifyWidgetResult $ widgetResize (generifyEnv env) (generifyNode node) a b
        , widgetRender = \env node -> widgetRender (generifyEnv env) (generifyNode node)
        }

generifyWidgetResult :: (Typeable model, Typeable event) => WidgetResult model event -> WidgetResult model (GenericAppEvent model event)
generifyWidgetResult WidgetResult{..} =
    WidgetResult
        { _wrNode = generifyNode _wrNode
        , _wrRequests = generifyWidgetRequest <$> _wrRequests
        }

ungenerifyWidgetResult :: (Typeable model, Typeable event) => WidgetResult model (GenericAppEvent model event) -> WidgetResult model event
ungenerifyWidgetResult WidgetResult{..} =
    WidgetResult
        { _wrNode = ungenerifyNode _wrNode
        , _wrRequests = ungenerifyWidgetRequest <$> _wrRequests
        }

generifyWidgetRequest :: (Typeable model, Typeable event) => WidgetRequest model event -> WidgetRequest model (GenericAppEvent model event)
generifyWidgetRequest (RaiseEvent e) = RaiseEvent $ Send e
generifyWidgetRequest r = unsafeCoerce r

ungenerifyWidgetRequest :: (Typeable event) => WidgetRequest model (GenericAppEvent model event) -> WidgetRequest model event
ungenerifyWidgetRequest (RaiseEvent (Send e)) = RaiseEvent e
ungenerifyWidgetRequest (RaiseEvent _) = error "You donkey"
ungenerifyWidgetRequest r = unsafeCoerce r

ungenerifyNode :: (Typeable model, Typeable event) => WidgetNode model (GenericAppEvent model event) -> WidgetNode model event
ungenerifyNode WidgetNode{..} = WidgetNode
    { _wnChildren = ungenerifyNode <$> _wnChildren
    , _wnWidget = ungenerifyWidget _wnWidget
    , ..
    }

ungenerifyEnv :: (Typeable model, Typeable event) => WidgetEnv model (GenericAppEvent model event) -> WidgetEnv model event
ungenerifyEnv WidgetEnv{..} = WidgetEnv{_weWidgetKeyMap = ungenerifyNode <$> _weWidgetKeyMap,..}

generifyEnv :: (Typeable model, Typeable event) => WidgetEnv model event -> WidgetEnv model (GenericAppEvent model event)
generifyEnv WidgetEnv{..} = WidgetEnv{_weWidgetKeyMap = generifyNode <$> _weWidgetKeyMap,..}

generifyConfigs :: [AppConfig model event] -> [AppConfig model (GenericAppEvent model event)]
generifyConfigs = (Monomer.appInitEvent Init :) . fmap generifyConfig

generifyConfig :: AppConfig model event -> AppConfig model (GenericAppEvent model event)
generifyConfig AppConfig{..} =
    AppConfig
        { _apcInitEvent = Send <$> _apcInitEvent
        , _apcDisposeEvent = Send <$> _apcDisposeEvent
        , _apcExitEvent = Send <$> _apcExitEvent
        , _apcResizeEvent = (Send .) <$> _apcResizeEvent
        , ..
        }
