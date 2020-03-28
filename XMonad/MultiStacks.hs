module XMonad.MultiStacks where

import Control.Monad (when)
import Data.Typeable
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

newtype MultiStack a = MultiStack { getStacks :: [Stacks a] }
type Stacks a = [Maybe (W.Stack a)]

instance Typeable (MultiStack a) where
    typeOf _ = "MultiStack"
instance ExtensionClass (MultiStack a) where
    initialValue = MultiStack []

-- switchToStack :: Int -> X ()
-- switchToStack i = do

-- get stack, returning default stack on failure
getStack :: Int -> X (W.Stack a)
getStack i = do
    MultiStack stacks <- XS.get :: X (MultiStack a)

    -- guard against empty state
    if (null stacks) then
        -- initialize state
        -- mkStacks >> getStack i
        return ()
    else
        when (length stacks <= i) $ insertStack i emptyStack

insertStack :: Int -> Stacks a -> X ()
insertStack i s = do
    MultiStack stacks <- XS.get :: X (MultiStack a)
    -- TODO insertAt
    XS.put $ MultiStack (s:stacks)

emptyStack :: X (Stacks a)
emptyStack = do
    MultiStack stacks <- XS.get :: X (MultiStack a)
    
    -- guard against empty state
    if (null stacks) then do
        l <- withWindowSet $ return . length . W.workspaces
        return (replicate l Nothing)
    else do
        let s = head stacks
            l = length s
        return (replicate l Nothing)

