module Todo.TodoForm where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Example.Library (MyAppProps, myAppPropsFromJS, theMyAppClass)
import Example.LoggingFn (logTrue)
import React.Basic (ReactComponent, Self, toReactComponent)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Events as Event
import React.Basic.Events (EventHandler)
import Todo.TodoLibrary (StateTransFn, componentPropsFor, myTodoAddWord, myTodoSetSearchString, myTodoView, myTodoDeleteSelected)
import Todo.TodoState (TodoState(..), myTodoInitState)

newMyTodoApp :: ReactComponent MyAppProps
newMyTodoApp = toReactComponent myAppPropsFromJS theMyAppClass
  { initialState: myTodoInitState
  , render: \ self ->
    let cprops = componentPropsFor self in -- 'componentPropsFor' is defined below.
    DOM.div_
    [ DOM.div_
      [ DOM.span_ [DOM.text "Enter Todo:"]
      , DOM.input
        { onChange: textEntryEventHandler self "search" myTodoSetSearchString
        ,  onSubmit: submitEventHandler self
        }
      ]
      , DOM.div_
      [ DOM.button
        { children: [DOM.text "Add"]
        , onClick: submitEventHandler self
        }
        , DOM.button
        { children: [DOM.text "Delete"]
        , onClick: Event.capture_ $ do
            self.setState $ unwrap $ myTodoDeleteSelected
            logTrue "Deleted selected items"
        }
      ]
      
    , myTodoView cprops
      -- Here we pass 'cprops' to allow stateless components access to the 'state' and 'setState'
      -- function of this main app component.
    ]
  }

-- | This is the submit event handler.
submitEventHandler :: Self {} TodoState -> EventHandler
submitEventHandler self = Event.capture_ $ do
  let (TodoState state) = self.state
  self.setState $ unwrap myTodoAddWord -- Here we provide a 'StateTransFn'
  logTrue $ "Add Todo " <>
    show (state.param1) <> " = " <>
    show (state.param2)

-- | This is the text input event handler. This function can take one of two 'StateTransFn', which
-- could be 'myAppSetSearchString' or 'myAppSetDefinitionString'. In this way, we can parameterize
-- the event handler over the state transition function.
textEntryEventHandler
  :: Self {} TodoState
  -> String -- ^ indicate which 'StateTransFn' we are using to report in the log message
  -> (Maybe String -> StateTransFn TodoState)
  -> EventHandler
textEntryEventHandler self logWhich stransf = Event.capture targetValue $ \ inputString -> do
  self.setState $ unwrap $ stransf inputString
  logTrue $ "Set " <> logWhich <> " string to: " <> show inputString