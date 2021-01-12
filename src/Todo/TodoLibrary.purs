module Todo.TodoLibrary where
  
import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.String.CodeUnits as Code
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import React.Basic (Component, JSX, Self, createComponent, makeStateless)
import React.Basic.DOM as DOM
import Todo.TodoState (TodoList, TodoState(..))

-- State transition abstractions:
--
-- This will eventually go in a separate module.

newtype StateTransFn a = StateTransFn (a -> a)
  
instance composeStateTransFn :: Semigroup (StateTransFn a) where
  append (StateTransFn a) (StateTransFn b) = StateTransFn (a >>> b)

instance emptyStateTransFn :: Monoid (StateTransFn a) where
  mempty = StateTransFn identity

-- Let's also instantiate 'wrap' and 'unwrap' from the 'Data.Newtype' module.
instance newtypeStateTransFn :: Newtype (StateTransFn a) (a -> a) where
  unwrap (StateTransFn f) = f
  wrap = StateTransFn

-- | This is a constructor for a 'StateTransFn' that automatically unwraps and rewrites a 'Newtype'
-- state, apply the unwrapped record type value to the given function. This is more convenient as it
-- allows you to update the state without unwrapping and re-wrapping it yourself.
stateTransFn
  :: forall outer inner . Newtype outer inner
  => (inner -> inner)
  -> StateTransFn outer
stateTransFn f = StateTransFn (unwrap >>> f >>> wrap)


 -- | This is the data type we will use for all statelses React components -- let's call these
-- stateless components "views", and the 'ViewProp's contain the properties the view needs to
-- render, including the entire current state of the main component.
-- 
-- React-Basic requires all components constructed with 'makeStateless' take a "properties"
-- value. For the main component (which is not stateless), both properties and state are contained
-- in a record type called Self (see 'React.Basic.Self'). A 'Self' data type cannot contain itself,
-- but we can construct a type that contains the two properties we care about, which are 'state' and
-- 'setState', along with the props for the stateles component.
type ViewTodoProps props state =
    { state    :: state
    , setState :: StateTransFn state -> Effect Unit
    , props    :: props
    }

-- | This function takes a 'Self' value, which is provided by the React Basic environment whenever
-- an event takes place in the DOM, and extracts the two fields that are relevant for use by our
-- stateless view-only React components. The 'ViewProps' type can be used as the "props" type
-- for any stateless component.
componentPropsFor :: forall props state . Self props state -> ViewTodoProps props state
componentPropsFor self =
  { props: self.props
  , state: self.state
  , setState: unwrap >>> self.setState
  } 

-- State Transition Functions

-- Now let's define the state transitions that can occur for this 'MyAppState'. The 'StateTransFn'
-- data type will be defined below, at the end of this module. For now, let's just assume that we
-- need to define functions of type 'StateTransFn' in order to update the state of our app.

-- | Add an element to the dictionary
myTodoAddWord :: StateTransFn TodoState
myTodoAddWord = stateTransFn $ \ this ->
  this
  { todosArray =
    Array.cons
    { myText  : this.param1
    , myStatus: this.param2
    , mySelect: false 
    }
    this.todosArray
  }

myTodoDeleteSelected :: StateTransFn TodoState
myTodoDeleteSelected = stateTransFn $ \ this ->
  this
  { todosArray =
    Array.filter (\ entry -> not entry.mySelect) this.todosArray
  }  

myTodoSetSearchString :: Maybe String -> StateTransFn TodoState
myTodoSetSearchString mquery = stateTransFn $ \ this ->
  this
  { param1 = fromMaybe "" mquery
  }

newtype MyTodoEntryProps = MyTodoEntryProps MyTodoEntryPropsRec  
--derive instance newtypeMyTodoEntryProps :: Newtype MyTodoEntryProps _

type MyTodoEntryPropsRec =
    { i     :: Int
    , entry :: TodoList
    }

myTodoEntryViewClass :: Component (ViewTodoProps MyTodoEntryProps TodoState)
myTodoEntryViewClass = createComponent "MyTodoEntryView"

myTodoEntryView :: ViewTodoProps MyTodoEntryProps TodoState -> JSX
myTodoEntryView = makeStateless myTodoEntryViewClass $ \ cprops ->
  let (MyTodoEntryProps props) = cprops.props in
  let (TodoState state) = cprops.state in
  DOM.li
  { className:
    if props.entry.mySelect then "dict-entry-view-selected" else "dict-entry-view"
  , children:
    [ DOM.span
      { className: "entry-Text"
      , children: [DOM.text props.entry.myText]
      }
    ]  
  }  

myTodoViewClass :: Component (ViewTodoProps {} TodoState)
myTodoViewClass = createComponent "MyTodoView"

myTodoView :: ViewTodoProps {} TodoState ->JSX
myTodoView = makeStateless myTodoViewClass  $ \ cprops ->
  let (TodoState state) = cprops.state in
  DOM.ul
  { className: "dictionary-view"
  , children:
    ( Array.mapWithIndex -- construct the list of elements to be rended
      (\ i entry ->
         { jsx: myTodoEntryView (cprops{ props = MyTodoEntryProps {i, entry}})
         , myText: entry.myText
         }
      ) >>>
      ( if String.null state.param1 then identity else
        Array.filter -- remove elements that do not match the query string
        (\ jsxmyText -> Code.contains (Pattern state.param1) jsxmyText.myText
        )
      ) >>>
      map (\ jsxmyText -> jsxmyText.jsx) -- extract the JSX values
    ) $
    state.todosArray
    
  }
  