module Library.StateTransition where

import Prelude
import Data.Newtype (class Newtype, unwrap, wrap)

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