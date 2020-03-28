module XMonad.PreferredView (viewOnPreferredScreen, onPreferredScreen, onMainScreen) where

import XMonad.Actions.OnScreen
import XMonad.Core
import XMonad.StackSet
import qualified Data.List as L

-- type PreferredScreen = Maybe ScreenId
-- type PreferredScreens = [(WorkspaceId, ScreenId)]

-- helper to override view to prefer screen id in preferredScreens
viewOnPreferredScreen :: [(WorkspaceId, ScreenId)] -> WorkspaceId -> WindowSet -> WindowSet
viewOnPreferredScreen = flip onPreferredScreen view

-- helper which accepts a view function to use to view on a preferred screen
-- function of windowset -> preferred workspace & screens -> workspace ID -> WindowSet
onPreferredScreen
    :: [(WorkspaceId, ScreenId)]
        -> (WorkspaceId -> WindowSet -> WindowSet)
        -> WorkspaceId
        -> WindowSet
        -> WindowSet
onPreferredScreen preferredScreens f tag =
  let lookupWorkspace   = lookup tag preferredScreens
      viewOnScreen' s i = onScreenIfExists (f i) tag s
  in maybe f viewOnScreen' lookupWorkspace tag

-- helper which accepts a view function to use to view on the main screen, and another to use when not on main screen
onMainScreen
    :: (WorkspaceId -> WindowSet -> WindowSet)
        -> (WorkspaceId -> WindowSet -> WindowSet)
        -> WorkspaceId
        -> WindowSet
        -> WindowSet
onMainScreen f f' tag ws = if screen (current ws) == S 0 then f tag ws else f' tag ws

-- this function acts on the screen something only when the screen exists,
-- otherwise just delegate to view function
onScreenIfExists :: (WindowSet -> WindowSet) -> WorkspaceId -> ScreenId -> WindowSet -> WindowSet
onScreenIfExists f i s ws = if screenExists then onScreen f (FocusTag i) s ws else f ws
    where screenExists = s `elem` map screen (screens ws)

-- TODO this isn't very helpful...
-- more advanced helper, which prefers preferredScreens
-- but allows viewing alt screens side by side if focusing alt screen
viewOnPreferredScreenAdv :: [(WorkspaceId, ScreenId)] -> WorkspaceId -> WindowSet -> WindowSet
viewOnPreferredScreenAdv preferredScreens tag ws =
  let
    isAltScreen = maybe False isAltScreen' . screenIndex
      where
        isAltScreen' i = maybe False (/= i) curIndex
        curIndex       = screenIndex focusedTag
        screenIndex  t = lookup t preferredScreens
    otherScreen   = if focusedScreen > 0 then S 0 else focusedScreen + 1
    otherTag      = maybe focusedTag id $ lookupWorkspace otherScreen ws
    focusedScreen = screen $ current ws
    focusedTag    = currentTag ws
  in
    if isAltScreen tag then
      viewOnScreen focusedScreen tag . viewOnScreen otherScreen otherTag $ ws
    else
      viewOnPreferredScreen preferredScreens tag ws

