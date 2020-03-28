{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.HideMatchingWindows (hideApp, hideTitle, hideClass) where

import Control.Monad (filterM)
import Data.Functor
import Data.Maybe (isJust, fromJust)
import Debug.Trace
import XMonad hiding ( (|||), defaultConfig, focus )
import XMonad.Layout.LayoutModifier
import XMonad.Util.Operations
import qualified XMonad.StackSet as W

data HideMatchingWindows a = HideMatchingWindows (HideMatchingQuery a) (Maybe (W.Stack a)) deriving (Show, Read)
data HideMatchingQuery   a = HideApp String | HideTitle String | HideClass String deriving (Show, Read)

hideApp   s = ModifiedLayout (HideMatchingWindows (HideApp   s) Nothing)
hideTitle s = ModifiedLayout (HideMatchingWindows (HideTitle s) Nothing)
hideClass s = ModifiedLayout (HideMatchingWindows (HideClass s) Nothing)

instance LayoutModifier HideMatchingWindows Window where
    modifyLayoutWithUpdate (HideMatchingWindows m prev) w@(W.Workspace _ _ s) r = do
            hidden <- filterM (runQuery (hideQuery m)) (W.integrate' s)
            dir    <- focusDirX prev

            let filter = W.filter (not . (`elem` hidden))
                s' = do
                    foc      <- fmap W.focus s

                    -- if hidden window is focused, focus the next/prev non-hidden window
                    -- FIXME not changing focus properly
                    -- FIXME not restoring after switching to different tag
                    if foc `elem` hidden then do
                        case dir of
                            FocusUp   -> filter . W.focusUp' =<< s
                            FocusDown -> filter . W.focusDown' =<< s
                            FocusStay -> filter =<< s
                    else
                        filter =<< s

            ret  <- runLayout (w { W.stack = s' }) r
            next <- W.stack . W.workspace . W.current <$> gets windowset

            return (ret, Just (HideMatchingWindows m next))
        where
            hideQuery (HideApp   s) = appName   =? s
            hideQuery (HideTitle s) = title     =* s
            hideQuery (HideClass s) = className =? s

data FocusDir = FocusUp | FocusDown | FocusStay deriving Show

-- get focusdir for prev stack in X monad
focusDirX :: Maybe (W.Stack Window) -> X (FocusDir)
focusDirX s = do
    s' <- W.stack . W.workspace . W.current <$> gets windowset
    case (s, s') of
        (Just s, Just s') -> return (focusDir s s')
        otherwise         -> return FocusStay

-- get focusdir for prevStack -> curStack
focusDir :: Eq a => W.Stack a -> W.Stack a -> FocusDir
focusDir (W.Stack f us ds) (W.Stack f' us' ds')
    | f /= f' && (length us > length us' || (length ds' == 0 && length us == 0)) = FocusUp
    | f /= f' && (length ds > length ds' || (length us' == 0 && length ds == 0)) = FocusDown
    | otherwise = FocusStay
