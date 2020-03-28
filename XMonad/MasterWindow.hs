{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.MasterWindow (avoidMaster, avoidFocus, forceMaster, forceSecondary, forceLast, avoidApp, avoidTitle, avoidClass, avoidAppReversed, avoidTitleReversed, avoidClassReversed) where

import Data.Functor
import Data.Monoid (appEndo, mempty)
import XMonad hiding ( (|||), defaultConfig, focus )
import XMonad.Layout.LayoutModifier
import XMonad.Util.Operations
import qualified XMonad.StackSet as W

-- TODO implement avoid(Master|Focus) managehook in the LayoutModifier

-- Avoid the master window, but otherwise manage new windows normally.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' avoidMasterOnStack

-- similar to avoidMaster but put window below focus window
avoidFocus :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidFocus = W.modify' $ \(W.Stack t us (d:ds)) -> (W.Stack t (d:us) ds)

avoidMasterOnStack :: W.Stack a -> W.Stack a
avoidMasterOnStack (W.Stack t [] (r:rs)) = W.Stack t [r] rs
avoidMasterOnStack s = s

avoidMasterOnStackReversed :: W.Stack a -> W.Stack a
avoidMasterOnStackReversed (W.Stack t [] ds) =
    let reversed = avoidMasterOnStack (W.Stack t [] (reverse ds))
    in  W.Stack t (W.up reversed) (reverse (W.down reversed))
avoidMasterOnStackReversed s = s

-- Force the current focus to master window, but otherwise manage new windows normally.
forceMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
forceMaster = W.shiftMaster

-- Force the current focus underneath master window, but otherwise manage new windows normally.
forceSecondary :: W.StackSet i l a s sd -> W.StackSet i l a s sd
forceSecondary = W.modify' $ \s -> case s of
    W.Stack f [] (r:rs) -> W.Stack f [r] rs
    W.Stack f up rs -> let (u:ps) = reverse up in W.Stack f [u] (ps ++ rs)

-- Force the current focus to last window, but otherwise manage new windows normally.
forceLast :: W.StackSet i l a s sd -> W.StackSet i l a s sd
forceLast = W.modify' $ \(W.Stack f up rs) -> W.Stack f (reverse rs ++ up) []

-- for Avoid master window LayoutModifier
data AvoidMaster a = AvoidApp Mode String | AvoidTitle Mode String | AvoidClass Mode String deriving (Show, Read)
data Mode = Normal | Reversed deriving (Show, Read)

avoidApp   = ModifiedLayout . AvoidApp Normal
avoidTitle = ModifiedLayout . AvoidTitle Normal
avoidClass = ModifiedLayout . AvoidClass Normal

-- these are needed to work with stuff like MagicFocus
avoidAppReversed   = ModifiedLayout . AvoidApp Reversed
avoidTitleReversed = ModifiedLayout . AvoidTitle Reversed
avoidClassReversed = ModifiedLayout . AvoidClass Reversed

instance LayoutModifier AvoidMaster Window where
    modifyLayout m workspace r = do
            let q = avoidQuery m --> doF (avoidAction m)
                foc = W.focus <$> W.stack workspace
            k <- maybe (return mempty) (runQuery q) foc

            let stack' = maybe Nothing (Just . appEndo k) (W.stack workspace)
                workspace' = workspace { W.stack = stack' }

            runLayout workspace' r
        where
            avoidQuery :: AvoidMaster Window -> Query Bool
            avoidQuery (AvoidApp   _ s) = appName =? s
            avoidQuery (AvoidTitle _ s) = title =* s
            avoidQuery (AvoidClass _ s) = className =? s

            avoidAction :: AvoidMaster Window -> (W.Stack a -> W.Stack a)
            avoidAction (AvoidApp   Normal   _) = avoidMasterOnStack
            avoidAction (AvoidTitle Normal   _) = avoidMasterOnStack
            avoidAction (AvoidClass Normal   _) = avoidMasterOnStack
            avoidAction (AvoidApp   Reversed _) = avoidMasterOnStackReversed
            avoidAction (AvoidTitle Reversed _) = avoidMasterOnStackReversed
            avoidAction (AvoidClass Reversed _) = avoidMasterOnStackReversed
