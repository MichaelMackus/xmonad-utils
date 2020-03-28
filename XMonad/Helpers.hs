module XMonad.Helpers where

import Control.Applicative
import Data.Char (toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Numeric (showHex)
import XMonad hiding ( (|||) )

----------------------
-- helper operators --
----------------------

-- flipped fmap for helpful querying, e.g. className =$ (isPrefixOf "xmonad" . map toLower) --> title
(=$) :: Eq a => Query a -> (a -> Bool) -> Query Bool
q =$ f = fmap f q

-- matches any part of a query string, e.g. title =* "Google" --> title
(*=) :: Eq a => Query [a] -> [a] -> Query Bool
q *= x = q =$ (isInfixOf x)

-- matches start of a query string, e.g. className ^= "Firefox" --> title
(^=) :: Eq a => Query [a] -> [a] -> Query Bool
q ^= x = q =$ (isPrefixOf x)

qin :: Eq a => Query [a] -> [[a]] -> Query Bool
qin q xs = or <$> mapM (q *=) xs

-- matches any common window properties (seperated by | , with (windowId) as suffix)
properties :: Query String
properties = combine [title, appName, className] <+> windowId
    -- wrap windowId as hex value (e.g. 0xffffff) for parsing with x cli utils
    where windowId      = (wrap " (0x" ")" . ($"") . showHex) <$> ask
          wrap c c2 str = c ++ str ++ c2

-- combines any window query strings (seperated by | )
combine :: [Query String] -> Query String
combine props = combineStr " | " <$> sequence props

-- combines any strings, stripping empty ones
combineStr :: String -> [String] -> String
combineStr i = intercalate i  . stripEmpty
    where stripEmpty  = filter (not . null)

-- lower cases query string
qlower :: Query String -> Query String
qlower q = map toLower <$> q

