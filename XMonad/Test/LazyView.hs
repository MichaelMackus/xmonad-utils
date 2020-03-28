module XMonad.Test.LazyView where

import Control.Applicative
import XMonad.Actions.PhysicalScreens (viewScreen, PhysicalScreen(P))
import XMonad.Core
import XMonad.Operations
import XMonad.Test
import XMonad.LazyView
import qualified XMonad.StackSet as W

doLazyView = windows . lazyView

testView :: WorkspaceId -> IO XState
testView i = testXState $ windows (W.view i)

testView2 :: WorkspaceId -> WorkspaceId -> IO XState
testView2 i i' = testXState $ (windows (W.view i)) >> (windows (W.view i'))

testLazy :: WorkspaceId -> IO XState
testLazy i = testXState $ doLazyView i

testLazy2 :: WorkspaceId -> WorkspaceId -> IO XState
testLazy2 i i' = testXState $ (doLazyView i) >> (doLazyView i')

testLazy3 :: WorkspaceId -> WorkspaceId -> WorkspaceId -> IO XState
testLazy3 i i' i'' = testXState $ (doLazyView i) >> (doLazyView i') >> (doLazyView i'')

testLazyScrs :: WorkspaceId -> WorkspaceId -> IO XState
testLazyScrs i i' = testXState $ (doLazyView i) >> (viewScreen $ P 1) >> (doLazyView i')

testLazyScrs3 :: WorkspaceId -> WorkspaceId -> WorkspaceId -> IO XState
testLazyScrs3 i i' i'' = testXState $ (doLazyView i) >> (viewScreen $ P 1) >> (doLazyView i') >> (viewScreen $ P 0) >> (doLazyView i'')

