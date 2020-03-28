{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.MagicFocus (magicFocus) where

import Control.Applicative
import XMonad hiding ( (|||), defaultConfig, focus )
import XMonad.Layout.LayoutModifier
import qualified Data.List as L
import qualified XMonad.StackSet as W

data MagicFocus a = MagicFocus | MagicFocused Window (MagicFocus a) deriving (Show, Read)

-- TODO implement Position and Focus
data Position = Above | Below
data Focus    = Newer | Older

instance LayoutModifier MagicFocus Window where
    modifyLayoutWithUpdate m w@(W.Workspace _ _ s) r =
        let w' = w { W.stack = maybe Nothing (`stackf` m) s }
            f  = maybe Nothing (\(W.Stack f _ _) -> Just f) s

            stackf s@(W.Stack f us ds) m =
                case m of
                    MagicFocus       -> forceMaster s
                    MagicFocused f k -> maybe (forceMaster s) Just $ do
                        f' <- L.find (== f) (W.integrate s)
                        if (notNextFocus f' s) then
                            stackf s k
                        else
                            Nothing

            forceMaster   (W.Stack f us ds)  = Just (W.Stack f [] (ds ++ reverse us))

        in  do
            r <- runLayout w' r
            return (r, (\f -> MagicFocused f MagicFocus) <$> f)

magicFocus = ModifiedLayout (MagicFocus)

-- magicFocus' pos foc = ModifiedLayout (MagicFocus)

-- don't swap focus when next/prev viewed
notNextFocus :: Eq a => a -> W.Stack a -> Bool
notNextFocus f (W.Stack f' us ds) = f /= f' && (length us > 0) && (us !! 0 /= f) && (length ds > 0) && (ds !! 0 /= f)
