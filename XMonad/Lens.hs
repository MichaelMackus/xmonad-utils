{-# LANGUAGE RankNTypes, TupleSections #-}

module XMonad.Lens where

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Monoid hiding (Product)
import Data.Traversable (traverse)
import Control.Applicative
import qualified Data.List as L

import XMonad
import qualified XMonad.StackSet as W

-- XMonad-specific lenses!

_workspaces :: Traversal' WindowSet (W.Workspace WorkspaceId (Layout Window) Window)
_workspaces f s = updateWS <$> traverse f (W.workspaces s)
    where
        updateWS ws = s {
                        W.current = (W.current s) { W.workspace = c },
                        W.visible = map (updateV vis) (zip [0..] $ W.visible s),
                        W.hidden = map (updateH hid) (zip [0..] $ W.hidden s)
                      }
            where
                c
                  | length ws == 0 = error "Empty workspaces passed to _workspaces"
                  | otherwise = head ws

                vis
                  -- guard for safe length
                  | length vis' /= length (W.visible s) = error "Visible workspaces length mismatch"
                  | otherwise = vis'
                hid
                  -- guard for safe length
                  | length hid' /= length (W.hidden s) = error "Hidden workspaces length mismatch"
                  | otherwise = hid'

                vis' = take (length $ W.visible s) (drop 1 ws)
                hid' = drop (1 + length vis) ws

        -- update f TODO maybe we can use other lenses here?
        updateV vis (i, scr)
            | length vis <= i = error "Error updating visible workspaces - length mismatch"
            | otherwise       = scr { W.workspace = vis !! i }

        updateH hid (i, ws)
            | length hid <= i = error "Error updating hidden workspaces - length mismatch"
            | otherwise       = hid !! i


-- TODO perhaps make these functions update hidden(s)/visible(s) ??

_screens :: Traversal' WindowSet (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
_screens f s = updateScrs <$> traverse f (W.screens s)
    where
        updateScrs []      = error "Empty screens passed to _screens"
        updateScrs (c:vis) = s { W.current = c, W.visible = vis }

_current :: Lens' WindowSet (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
_current f s = updateScr <$> f (W.current s)
    where updateScr c = s { W.current = c }

_currentTag :: Lens' WindowSet WorkspaceId
_currentTag f = _currentWS f'
    where f' ws = (\i -> ws { W.tag = i }) <$> f (W.tag ws)

_currentWS :: Lens' WindowSet (W.Workspace WorkspaceId (Layout Window) Window)
_currentWS f s = updateWS <$> f curws
    where
        updateWS ws = s { W.current = curscr { W.workspace = ws } }
        curscr      = W.current s
        curws       = W.workspace curscr

_visible :: Traversal' WindowSet (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
_visible f s = updateScrs <$> traverse f (W.visible s)
    where updateScrs vis = s { W.visible = vis }

-- traversal over visible WSes - this also updates the hidden WSes from previous visible
_visibleWS :: Traversal' WindowSet (W.Workspace WorkspaceId (Layout Window) Window)
_visibleWS f s = updateHid <$> updateVis <$> traverse f (map W.workspace $ W.visible s)
    where
      updateVis visWS = s { W.visible = map setWS (zip [0..] $ W.visible s) }
        where
          setWS  (i,scr)
            | length visWS <= i = error "Error updating visible workspaces - length mismatch"
            | otherwise = scr { W.workspace = visWS !! i }

      updateHid   s' = s' { W.hidden = addNewHid $ deleteFirstsByTag (W.hidden s') visWS }
        where
          addNewHid h = newHid ++ h
          newHid      = deleteFirstsByTag (map W.workspace $ W.visible s) visWS
          visWS       = map W.workspace $ W.visible s'
          deleteFirstsByTag = L.deleteFirstsBy (\a b -> W.tag a == W.tag b)

_hidden :: Traversal' WindowSet (W.Workspace WorkspaceId (Layout Window) Window)
_hidden f s = updateHidden <$> traverse f (W.hidden s)
    where updateHidden h = s { W.hidden = h }

-- boilerplate

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

type GetSet  s t a b = forall x. (a -> Product (Const x) Identity b) -> s -> Product (Const x) Identity t
type Getter  s a = forall r. Getting r s a

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> set s <$> f (get s)

-- Make a simple getter lens (can be passed to anything that accepts a Getting * s a)
to :: (s -> a) -> Getter s a
to getter f s = Const . getConst $ f (getter s)

-- Modify the target of a lens and return the result.
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l $ (,) <$> f <*> f

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l $ (,) <$> id <*> f

-- View contents of a lens.
view :: Getting a s a -> s -> a
view l s = getConst (l Const s)

-- Modify the lens using a pure function.
over :: Setting s t a b -> (a -> b) -> s -> t
over l f s = runIdentity $ l (Identity . f) s

-- Const over
set :: Setting s t a b -> b -> s -> t
set l b s = over l (const b) s

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (\x -> Const (First (Just x)))

has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (\x -> Const (Any True))
