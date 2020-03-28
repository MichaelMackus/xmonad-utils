{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module XMonad.Util.ToggleView (View(..), toggleViews, triggerView, nextView, prevView, curView) where

import Control.Monad (when)
import Data.Typeable (Typeable(..), mkTyCon3)
import Data.Typeable.Internal (Fingerprint(..), TypeRep(..))
import XMonad.Core
import XMonad.Operations (windows, refresh)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

-- ViewStack is a simple stack of Views, with current active view function at top.
-- View is simply a String label/identifier and a view function to apply to the windowset.
data ViewStack = ViewStack [View]
data View      = View String (WorkspaceId -> WindowSet -> WindowSet) | XView String (WorkspaceId -> X ())

instance Typeable (ViewStack) where
  typeOf _ = let tycon = mkTyCon3 "ToggleView" "XMonad.Util.ToggleView" "ViewStack"
             in TypeRep (Fingerprint 0 0) tycon []
instance ExtensionClass (ViewStack) where
  initialValue = ViewStack []

-- initialize toggleable views
toggleViews :: [View] -> X ()
toggleViews vs = XS.put (ViewStack vs)

-- helper shortcut - custom view function that uses top view on ViewStack
triggerView :: WorkspaceId -> X ()
triggerView i = curView >>= doView
    where
        -- run inner view function in X monad
        doView :: View ->  X ()
        doView (View  _ f) = windows (f i)
        doView (XView _ f) = f i

-- get current view
curView :: X View
curView = do
    s <- XS.get :: X (ViewStack)
    case s of
        ViewStack (f:fs) -> return f
        otherwise        -> return (View "" (\t s -> id s))

-- toggle next view
nextView :: X ()
nextView = do
    s <- XS.get :: X (ViewStack)
    case s of
        ViewStack (f:fs) -> XS.put (ViewStack (fs ++ [f]))
        otherwise        -> return ()
    refresh

-- toggle prev view
prevView :: X ()
prevView = do
    ViewStack fs <- XS.get :: X (ViewStack)
    when (not (null fs)) $ XS.put (ViewStack (last fs:init fs))
    refresh
