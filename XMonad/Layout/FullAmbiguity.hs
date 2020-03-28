module XMonad.Layout.FullAmbiguity where

-- FIXME not working since we don't have access to other screen Fullscreened windows

import XMonad hiding (trace)
import XMonad.Layout.NoBorders
import Data.List
import Data.Function (on)
import qualified XMonad.StackSet as W

import Debug.Trace

-- simple override to count Full layouts as singleton (essentially Combines OtherIndicated + FullAmbiguity)
data FullAmbiguity = FullAmbiguity deriving (Read, Show)

instance SetsAmbiguous FullAmbiguity where
    hiddens _ wset mst wrs = next \\ bordered ms
    -- hiddens _ wset mst wrs = trace (show mst) $ trace (show wrs) $ next
      where next = hiddens OtherIndicated wset mst wrs
            ms = filter (`elem` W.integrate' mst) $ map fst wrs

            -- native implementation of OtherIndicated ambiguity f
            -- ambiguous [w]
            --   | let nonF = map integrate $ W.current wset : W.visible wset
            --   , length (concat nonF) > length wrs
            --   , singleton $ filter (1==) $ map length nonF = [w]
            --   | singleton screens = [w]
            -- ambiguous _ = []

            -- floating = [ w |
            --             (w, W.RationalRect px py wx wy) <- M.toList . W.floating $ wset,
            --             px <= 0, py <= 0,
            --             wx + px >= 1, wy + py >= 1]

            -- FIXME this basically does same thing as OtherIndicated
            -- FIXME we need to detect full screen layouts/windows on other workspaces, which probably means we need the X monad :(
            -- FIXME OR a way to detect on the Full layout type in another Workspace
            -- FIXME this might not be necessary if we can somehow detect result of hiddens for each successive screen
            bordered [w]
              | let nonF = map integrate $ W.current wset : W.visible wset
              , length (concat nonF) > length wrs                 -- are there other windows?
              , not . singleton $ filter (1==) $ map length nonF  -- are there other full screen windows? TODO this doesn't really detect full screen, just stack
              , length wrs /= length (integrate $ W.current wset) -- are other windows hidden on current WS?
              , singleton wrs = [w]
            bordered _ = []

            integrate y = W.integrate' . W.stack $ W.workspace y

{--
 - Default implementation of SetsAmbiguous
 -
instance SetsAmbiguous Ambiguity where
    hiddens amb wset mst wrs
      | Combine Union a b <- amb = on union next a b
      | Combine Difference a b <- amb = on (\\) next a b
      | Combine Intersection a b <- amb = on intersect next a b
      | otherwise = tiled ms ++ floating
      where next p = hiddens p wset mst wrs
            nonzerorect (Rectangle _ _ 0 0) = False
            nonzerorect _ = True

            screens =
              [ scr | scr <- W.screens wset,
                      case amb of
                            Never -> True
                            _ -> not $ null $ integrate scr,
                      nonzerorect . screenRect $ W.screenDetail scr]
            floating = [ w |
                        (w, W.RationalRect px py wx wy) <- M.toList . W.floating $ wset,
                        px <= 0, py <= 0,
                        wx + px >= 1, wy + py >= 1]
            ms = filter (`elem` W.integrate' mst) $ map fst wrs
            tiled [w]
              | Screen <- amb = [w]
              | OnlyFloat <- amb = []
              | OtherIndicated <- amb
              , let nonF = map integrate $ W.current wset : W.visible wset
              , length (concat nonF) > length wrs
              , singleton $ filter (1==) $ map length nonF = [w]
              | singleton screens = [w]
            tiled _ = []
            integrate y = W.integrate' . W.stack $ W.workspace y
--}

fullWindows :: W.StackSet i (Layout Window) Window s sd -> [Window]
fullWindows (W.StackSet scr _ _ _) = if isFull (W.layout ws) then integrate ws else []
    where
        ws = W.workspace scr
        integrate = W.integrate' . W.stack

singleton :: [a] -> Bool
singleton = null . drop 1

isFull :: LayoutClass l a => l a -> Bool
isFull l = False
