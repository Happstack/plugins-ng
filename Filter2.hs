module Filter2 (lower, upper) where

import Data.Char (toLower, toUpper)

lower :: String -> String
lower = map toLower

upper :: String -> String
upper = map toUpper
