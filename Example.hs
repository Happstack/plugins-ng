{-# LANGUAGE TemplateHaskell #-}
module Main where

import Plugins
import qualified Filter as F

main :: IO ()
main =
    do let names = ['F.filter]
       ph <- newPluginsHandle
       reload ph names
       loop ph

loop :: PluginsHandle -> IO ()
loop ph =
  do putStrLn "Enter some text followed by <enter> or 'quit' to exit:"
     msg <- getLine
     case msg of
       "quit" -> return ()
       _ -> do
         fn <- ($(lookupName 'F.filter) ph)
         print (fn msg)
         loop ph
