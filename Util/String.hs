module Util.String (capitalize) where

import qualified Data.Char as Char

capitalize :: String -> String
capitalize []     = error "null str"
capitalize (x:xs) = Char.toUpper x : xs
