module Filter (filter) where

import Data.Char
import Prelude hiding (filter)
import Filter2 (lower, upper)

filter :: String -> String
filter = upper

