module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Example.Library (myAppInitProps)
import Example.LoggingFn (logFalse)
import React.Basic (element)

import React.Basic.DOM as DOM
import Todo.TodoForm (newMyTodoApp)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


main :: Effect Unit
main = do
  logFalse "Begin Facade main function"
  doc <- window >>= document
  logFalse "Get the element identified as \"container\" from the DOM of index.html"
  maybeContainer <- getElementById "container" $ toNonElementParentNode doc
  case maybeContainer of
    Nothing        -> throw "Container element not found"
   
    -- for todo app 
    Just container -> DOM.render (element newMyTodoApp myAppInitProps) container
    
   


 