module XMonad.Test.LazyViewed where

-- import Control.Applicative
-- import XMonad.Actions.PhysicalScreens (viewScreen, PhysicalScreen(P))
import XMonad.Core
import XMonad.Operations
import XMonad.Test
import XMonad.LazyViewed
import qualified Data.List as L
import qualified XMonad.StackSet as W

testLazyViewed :: [X ()] -> IO XState
testLazyViewed actions = testXState $ sequence_ (L.intersperse lazyViewHook actions)

testViews :: [WorkspaceId] -> IO XState
testViews = testLazyViewed . viewTags

viewTags :: [WorkspaceId] -> [X ()]
viewTags = map (windows . W.view)
