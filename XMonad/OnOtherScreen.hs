module XMonad.OnOtherScreen (viewOnOtherScreen, onOtherScreen, onMainScreen, doOnOtherScreen, onWorkspace) where

import Control.Applicative
import Data.Maybe
import XMonad
import XMonad.StackSet
import qualified Data.List as L

-- copied from XMonad.StackSet
onWorkspace :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
            -> StackSet i l a s sd -> StackSet i l a s sd
onWorkspace i f s = view (currentTag s) . f . view i $ s

-- modify workset on main screen
onMainScreen :: (Eq i, Eq s) => Bool -> (StackSet i l a s sd -> StackSet i l a s sd) -> StackSet i l a s sd -> StackSet i l a s sd
onMainScreen foc f s = focusScreen (f (mainScreen s))
    where focusScreen = if foc then mainScreen else otherScreen -- TODO otherScreen could be smarter

-- shift to visible screen, but keep current focus
viewOnOtherScreen :: (Eq i, Eq s) => i -> StackSet i l a s sd -> StackSet i l a s sd
viewOnOtherScreen i s =
    if isJust (L.find ((i==).tag) (hidden s)) then
        onOtherScreen (view i) s
    else s

onOtherScreen :: (StackSet i l a s sd -> StackSet i l a s sd) -> StackSet i l a s sd -> StackSet i l a s sd
onOtherScreen f s = otherScreen (f (otherScreen s))

doOnOtherScreen :: X a -> X a
doOnOtherScreen f = do
    windows otherScreen
    r <- f
    windows otherScreen
    return r

-- makes first other screen current
otherScreen :: StackSet i l a s sd -> StackSet i l a s sd
otherScreen s =
    if (not . null $ visible s) then
        s {
            current = head (visible s),
            visible = (current s):(drop 1 $ visible s)
        }
    else s

-- makes main screen current
mainScreen :: StackSet i l a s sd -> StackSet i l a s sd
mainScreen s =
    if (not . null $ visible s) then
        s {
            current = head (screens s),
            visible = drop 1 (screens s)
        }
    else s

