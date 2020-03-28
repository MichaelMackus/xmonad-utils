module XMonad.LazyView (lazyView) where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.State (gets, put)
import Data.Either
import Data.Maybe
import XMonad.Core hiding (workspaces, trace)
import XMonad.StackSet
import XMonad.Operations (windows)
import qualified Data.List as L
import qualified XMonad.Util.ExtensibleState as XS

-- similar to greedyView, but instead of swapping visible screens, shows last
-- focused workspace on the other screen(s)
lazyView :: WorkspaceId -> WindowSet -> WindowSet
lazyView i s = maybe (greedyView i) (lazyUpdate currentW) targetW $ s
  where currentW = workspace (current s)
        targetW  = do
            target <- L.find ((i==).tag) (hidden s)
            -- don't lazyView if current stack is empty
            if isJust (stack currentW) then
                return target
            else
                Nothing

-- update target workspace, to current, and prepend from workspace to visible
-- args: from -> target -> stackset
lazyUpdate :: (Eq s, Eq i) => Workspace i l a -> Workspace i l a -> StackSet i l a s sd -> StackSet i l a s sd
lazyUpdate from to s = s { current = (current s) { workspace = to }
        , visible = lazyVisible from (visible s)
        , hidden  = L.nubBy (equating tag) . L.deleteBy (equating tag) to $ lazyHidden from s }
    where equating f = \x y -> f x == f y

lazyVisible :: (Eq sid, Eq i) => Workspace i l a -> [Screen i l a sid sd] -> [Screen i l a sid sd]
lazyVisible w scs = map updateSc (zip [0..] scs)
    where updateSc (i, sc) = sc { workspace = newWs !! i }
          newWs            = w : map workspace scs

lazyHidden :: (Eq s, Eq i) => Workspace i l a -> StackSet i l a s sd -> [Workspace i l a]
-- lazyHidden w s = L.filter isHiddenWs (w:((workspace $ current s):((map workspace $ visible s) ++ hidden s)))
lazyHidden w s = L.filter isHiddenWs (w:workspaces s)
    where isHiddenWs w' = not (tag w' `elem` map (tag . workspace) newScs)
          newScs        = lazyVisible w (visible s)
