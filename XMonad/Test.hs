module XMonad.Test where

import Control.Applicative
import Control.Monad (filterM)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Data.Monoid (Monoid, mempty, appEndo)
import XMonad hiding ( (|||) )
import XMonad.Helpers
import XMonad.MasterWindow
import XMonad.OnOtherScreen
import XMonad.Util.Focus
import XMonad.Util.Operations
import qualified Data.Map as M
import qualified Data.Set as S
import qualified XMonad.StackSet as W

-------------------------
-- core test functions --
-------------------------

-- helper for testing X monad code, throwing away new state
testX :: X a -> IO (Maybe a)
testX = fmap fst . runXTest

-- helper for testing X monad code, throwing away new state
testX' :: X a -> IO a
testX' m = fromJust <$> testX m

-- helper for testing X monad code, returning new state
testXState :: X a -> IO XState
testXState = fmap snd . runXTest

testWS :: (WindowSet -> X a) -> IO a
testWS = testX' . withWindowSet

-- helper for testing X monad code
runXTest :: X a -> IO (Maybe a, XState)
runXTest f = do
    dpy  <- openDisplay ""
    conf <- defaultConf dpy
    st   <- defaultState dpy
    r    <- runX conf st (userCode f)

    closeDisplay dpy
    return r


-- wrapper for displaying tags from windowset
stateTags :: XState -> (WorkspaceId, [WorkspaceId], [WorkspaceId])
stateTags = wsTags . windowset

-- wrapper for displaying tags from windowset
wsTags :: WindowSet -> (WorkspaceId, [WorkspaceId], [WorkspaceId])
wsTags st = ( W.tag . W.workspace $ W.current st, map (W.tag . W.workspace) (W.visible st), map W.tag (W.hidden st))

-- helper for testing Query code
testQuery :: (Monoid a) => Query a -> Window -> IO a
testQuery f w = fromMaybe mempty <$> testX (runQuery f w)

-- map ManageHook over focused window
testManageHook :: ManageHook -> IO WindowSet
testManageHook mh = fmap windowset . testXState . withWindowSet $ \ws ->
    let foc  = W.focus <$> (curStack ws)
    in  maybe (return ws) (runManageHook mh) foc


-- helper for testing Query code on focused window
queryFocus :: (Monoid a) => Query a -> IO a
queryFocus f = do
    foc <- testX' focusedWin
    maybe (return mempty) ((fromMaybe mempty <$>) . testX . runQuery f) foc

-- helper function for running query (i.e. map reduce function) on all windows in X11
-- e.g. testX $ queryWindows (title =? "scratch" --> className)
-- (which should return Just ["tmux"])
queryWindows :: (Eq a, Monoid a) => Query a -> IO [a]
queryWindows f = toList . testX . withWindowSet $
        \ws -> stripEmpty <$> mapM (runQuery f) (W.allWindows ws)
    where
        toList     = fmap (fromMaybe [])
        stripEmpty = filter (/= mempty)


-- run a ManageHook over a window
runManageHook :: ManageHook -> Window -> X WindowSet
runManageHook mh w = do
    f  <- runQuery mh w
    ws <- gets windowset
    st <- get

    put $ st { windowset = appEndo f ws }
    gets windowset

-- get cur stack
getStack :: X (Maybe (W.Stack Window))
getStack = gets (curStack . windowset)

-- cur stack of WindowSet
curStack :: WindowSet -> Maybe (W.Stack Window)
curStack = W.stack . W.workspace . W.current

------------------------------
-- helper functions for X11 --
------------------------------

-- FIXME crashes ghci
testXEvent :: (Event -> IO ()) -> IO ()
testXEvent f = do
    testX' $ do
        dpy <- asks display
        foc <- focusedWin
        whenJust foc $ \w -> io $ do
            -- this seems to crash ghc...
            let ev = keyPressMask .|. buttonPressMask
            selectInput dpy w ev

            sync dpy True
            allocaXEvent $ \e -> do
                nextEvent dpy e
                getEvent e >>= f

-- get list of windows in x server
getWindows :: Display -> Window -> IO [Window]
getWindows disp root = do
    (_, _, ws) <- queryTree disp root
    return ws


----------------------
-- helper functions --
----------------------

focusedWins :: WindowSet -> [Window]
focusedWins ws = catMaybes $ map wsFocus (scrWs ++ W.hidden ws)
    where wsFocus ws = W.stack ws >>= return . W.focus
          scrWs      = map W.workspace (W.current ws : W.visible ws)

isFocused :: Query Bool
isFocused = ask >>= \w -> do
    foc <- liftX focusedWin
    return $ maybe False (== w) foc

-- helper for constructing default state
defaultState :: Display -> IO XState
defaultState dpy  = do
    ws <- defaultWS dpy
    return $ XState {
        windowset       = ws,
        mapped          = S.empty,
        waitingUnmap    = M.empty,
        dragging        = Nothing,
        numberlockMask  = noModMask,
        extensibleState = M.empty
    }

defaultConf :: Display -> IO XConf
defaultConf dpy = do
    let xmc = defaultConfig { layoutHook = Layout $ layoutHook defaultConfig }
    let dflt = defaultScreen dpy

    rootw  <- rootWindow dpy dflt
    nbc    <- do v            <- initColor dpy $ normalBorderColor  xmc
                 ~(Just nbc_) <- initColor dpy $ normalBorderColor defaultConfig
                 return (fromMaybe nbc_ v)

    fbc    <- do v <- initColor dpy $ focusedBorderColor xmc
                 ~(Just fbc_)  <- initColor dpy $ focusedBorderColor defaultConfig
                 return (fromMaybe fbc_ v)

    return $ XConf
        { display       = dpy
        , config        = xmc
        , theRoot       = rootw
        , normalBorder  = nbc
        , focusedBorder = fbc
        , keyActions    = keys xmc xmc
        , buttonActions = mouseBindings xmc xmc
        , mouseFocused  = False
        , mousePosition = Nothing
        , currentEvent  = Nothing }

-- helper for testing WindowSet
defaultWS :: Display -> IO WindowSet
defaultWS dpy = do
    xinesc <- getCleanedScreenInfo dpy
    rootw  <- rootWindow dpy $ defaultScreen dpy

    -- get list of windows in x server
    wins   <- getWindows dpy rootw

    let ws = W.new defaultLayout defaultWorkspaces $ map SD xinesc
    -- return ws
    -- TODO this crashes with some tests
    return ws {
        W.current = (W.current ws) { W.workspace = existingWorkspace wins },
        W.hidden  = (W.hidden ws)
    }
  where defaultLayout     = Layout $ layoutHook defaultConfig
        defaultWorkspaces = map show ([0..9])

        existingWorkspace wins = W.Workspace {
            W.tag = "X",
            W.layout = Layout Full,
            W.stack = Just $ W.Stack (head wins) [] (tail wins)
        }

