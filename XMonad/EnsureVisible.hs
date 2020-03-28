module XMonad.EnsureVisible (ensureVisible) where

import XMonad.Core
import XMonad.Layout
import XMonad.Layout.LimitWindows
import qualified XMonad.StackSet as W

-- Ensure the window is visible - if new focus pops it off the visible window
-- list, move it to last visible window
ensureVisible :: LayoutClass l a => W.StackSet i l a s sd -> W.StackSet i l a s sd
ensureVisible s = ensureVisibleOnLayout layout stack
    where stack  = W.stack ws
          layout = W.layout ws
          ws     = W.workspace (W.current s)

-- TODO pattern match for ModifiedLayout (LimitWindows s n)
ensureVisibleOnLayout :: LayoutClass l a => l a -> W.Stack a -> W.Stack a
ensureVisibleOnLayout (ModifiedLayout m l)    st = ensureVisibleOnLayout m st -- TODO or l (unlikely)
ensureVisibleOnLayout (LimitWindows FirstN n) st = ensureNVisible n st
ensureVisibleOnLayout _ st = st

ensureNVisible :: Int -> W.Stack a -> W.Stack a
ensureNVisible n (W.Stack f up rs) =
    let (up', xrs') = splitAt (n - 1) up
        rs' = xrs' ++ rs
    in W.Stack f up' rs'
ensureNVisible _ st = st

