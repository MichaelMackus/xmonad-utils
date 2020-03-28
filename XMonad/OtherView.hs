module XMonad.OtherView (viewOnOtherScreen, onOtherScreen) where

import XMonad.Stackset

viewOnOtherScreen :: (Eq s, Eq i) => i -> StackSet i l a s sd -> StackSet i l a s sd
viewOnOtherScreen = onOtherScreen view

onOtherScreen :: (Eq s, Eq i) => (i -> StackSet i l a s sd -> StackSet i l a s sd) -> i -> StackSet i l a s sd -> StackSet i l a s sd
onOtherScreen f = f
