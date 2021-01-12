module Todo.TodoState where

import Data.Newtype (class Newtype)
-- Here we define the data that contains the internal state of your program. In this example, it is
-- a simple dictionary relating word strings to definition strings.

-- | This could be a 'data TodoState' type as well, but I prefer to create a 'newtype' so that the
-- | record fields of the type do not get exported to the local namespace. This allows you to reuse
-- | the names of record fields for multiple data types.
newtype TodoState = TodoState TodoStateRec
derive instance newtypeTodoState :: Newtype TodoState _

-- | Here we have the record type for your App's state:
type TodoStateRec =
    { todosArray :: Array TodoList
    , param1     :: String
    , param2     :: String 
    }

type TodoList =
     { myText       :: String
     , myStatus     :: String   
     , mySelect     :: Boolean  
     }

myTodoInitState ::  TodoState
myTodoInitState =  TodoState
  { todosArray : todosArray
  , param1: ""
  , param2: ""     
  }   

todosArray :: Array TodoList
todosArray = []
  

  



