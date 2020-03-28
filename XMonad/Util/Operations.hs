module XMonad.Util.Operations where

import Graphics.X11.Types (Window)
import XMonad.Core
import XMonad.ManageHook (liftX, composeAll)
import qualified XMonad.StackSet as W

import Control.Applicative
import Control.Monad.State (gets)
import Data.Maybe
import Data.Monoid
import Text.Regex

(=*) :: Query String -> String -> Query Bool
(=*) q x = fmap matches q
    where
        matches = isJust . matchRegex regex
        regex   = mkRegex x

layoutTitle :: Query String
layoutTitle = description <$> liftX getLayout

-- FIXME this returns *all* layouts
getLayout :: X (Layout Window)
getLayout = do
    ws <- gets windowset
    let layout = W.layout (W.workspace (W.current ws))
    return layout

-- composeAll + map
composeWith :: (Monad m, Monoid a) => (m b -> a) -> [m b] -> a
composeWith f = composeAll . map f

-- flipped <$>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
