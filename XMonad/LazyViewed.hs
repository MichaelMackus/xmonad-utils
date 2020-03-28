{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.LazyViewed where

import Control.Monad (unless, when)
import Data.Maybe (isJust, fromJust)
import Data.Typeable
import Pager
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- similar structure to zipper: lefts -> rights
data LazyViewed = LazyViewed (Pager WindowSet) deriving Typeable

instance ExtensionClass (LazyViewed) where
    initialValue = LazyViewed (Pager Nothing [] [])

-- simple show instance for debugging
instance Show LazyViewed where
    show (LazyViewed pager) = show (fmap visibleTags (curPage pager), map visibleTags (prev pager), map visibleTags (next pager))

-- updates last viewed window in state
-- this should be called in a logHook in order to update the state every time
lazyViewHook :: X ()
lazyViewHook = do
    (LazyViewed pager) <- XS.get :: X (LazyViewed)
    ws                 <- gets windowset
    -- ensure not same as next or prev state state
    unless (viewed ws pager) $
        let pager' = setCurPage ws (lastPage pager)
        in  syncPage pager'

-- resets lazy viewed to last on next stack
resetViewed :: X ()
resetViewed = do
    (LazyViewed pager) <- XS.get :: X (LazyViewed)
    let pager' = lastPage pager
    when (isJust (curPage pager')) $ syncPage pager'

-- view previous viewed workspaces
lazyViewed :: X ()
lazyViewed = do
    (LazyViewed pager) <- XS.get :: X (LazyViewed)
    syncPage (prevPage pager)

-- view next viewed workspaces
nextViewed :: X ()
nextViewed = do
    (LazyViewed pager) <- XS.get :: X (LazyViewed)
    syncPage (nextPage pager)

-- TODO ensure there is a difference before syncing?
-- TODO maybe sync hidden WS order?
syncPage :: Pager WindowSet -> X ()
syncPage pager
    | isJust (curPage pager) = do
        (LazyViewed oldPager) <- XS.get :: X (LazyViewed)
        unless (oldPager `equals` pager) $ do
            XS.put (LazyViewed pager)

            -- trigger X11 updates (also triggers logHook)
            windows $ const (fromJust (curPage pager))
    | otherwise = return ()

-- helpers for simple Eq instance

equals :: Pager WindowSet -> Pager WindowSet -> Bool
equals pager pager' = toWS pager == toWS pager'

viewed :: WindowSet -> Pager WindowSet -> Bool
viewed ws pager = headOfPager (WS ws) (toWS pager)

data WS = WS WindowSet

toWS :: Pager WindowSet -> Pager WS
toWS (Pager cur p n) = Pager (fmap WS cur) (map WS p) (map WS n)

instance Eq WS where
    (WS ws) == (WS ws') = visibleTags ws == visibleTags ws'

visibleTags :: WindowSet -> [String]
visibleTags ws =
    let scrs ws' = (W.current ws'):(W.visible ws')
        tags     = map (W.tag . W.workspace)
    in  tags (scrs ws)

-- -- TODO FIXME is this even necessary?
-- viewedStack :: WindowSet -> W.StackSet WorkspaceId l a ScreenId sd -> W.StackSet WorkspaceId l a ScreenId sd
-- viewedStack viewed s =
--         let newCur = viewedScreen (W.screens s) (head viewed)
--             newVis = map (viewedScreen (W.screens s)) (drop 1 viewed)
--         in s {
--             W.current = fromMaybe (W.current s) newCur,
--             W.visible = maybeL (W.visible s) newVis
--         }
--     where
--         maybeL def xs = maybe def (const $ catMaybes xs) $ listToMaybe (catMaybes xs)

-- -- TODO FIXME
-- viewedScreen :: [W.Screen WorkspaceId l a ScreenId sd] -> (ScreenId, WorkspaceId) -> Maybe (W.Screen WorkspaceId l a ScreenId sd)
-- viewedScreen scrs (sid, i) = do
--     scr <- L.find ((sid==) . W.screen) scrs
--     ws  <- W.workspace <$> L.find ((i==) . W.tag . W.workspace) scrs
--     return $ scr { W.workspace = ws }
