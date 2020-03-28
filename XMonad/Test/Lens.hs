module XMonad.Test.Lens where

import Control.Applicative
import Data.Functor.Compose

import XMonad.Lens
import XMonad.Test
import qualified XMonad.StackSet as W

incWS :: W.Workspace String l a -> W.Workspace String l a
incWS ws = ws { W.tag = show ((read $ W.tag ws) + 1) }

-- validates composition law for traversalables
-- Law: traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
validateT :: (Eq s, Applicative f, Eq (f s)) => Traversal' s a -> (a -> f a) -> (a -> f a) -> s -> f Bool
validateT t f g s = liftA2 (==) t1 t2
    where
        t1 = getCompose . t (Compose . fmap g . f) $ s
        t2 = getCompose . Compose . fmap (t g) . t f $ s

-- validateT :: (Eq t, Applicative f) => Traversal s t a b -> (a -> f b) -> (a -> f b) -> s -> f Bool
-- validateT l f f' s = liftA2 (==) (l f s) (l f' s)
