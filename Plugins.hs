{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-

This module provides a system which can watch one or more symbols
which are loaded from local modules. If the files containing those
modules are changed, then the code is automatically recompiled and
loaded into the running process.

-}


module Plugins where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Monoid (mempty)
import Data.IORef
import Data.String (fromString)
import DynFlags
import Filesystem.Path (FilePath, dirname, filename)
import GHC
import GHC.Paths
import GhcMonad                   (liftIO) -- from ghc7.7 and up you can use the usual
import Language.Haskell.TH.Syntax as TH (Name(Name),NameFlavour(NameG), NameSpace(VarName), OccName(..), ModName(..))
import Module (moduleNameSlashes)
import System.FSNotify
import Unsafe.Coerce
import qualified Filter as F
import Prelude hiding (FilePath, filter)
import Language.Haskell.TH          (ExpQ, appE, varE)
import Language.Haskell.TH.Lift     (lift)

import HscTypes

{-

We need to watch a bunch of files and reload.

We need a way to map from a specific symbol to it's loaded value.

What happens when there is an error?

What happens when there are multiple symbols from the same file?

When a module is reloaded how do we ensure all the symbols get reloaded?

There are two phases:

 1. reloading all the modules

 2. evaluating the symbols

We can start with the brute force variant -- when any module is touched, we just reload everything.

-}

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

-- | extract the module name and occurance name of a symbol
--
-- can fail if the 'Name' is not for the right type of thing.
nameToModFunc :: TH.Name -> (ModuleName, String)
nameToModFunc (Name (OccName occName) (NameG VarName _ (ModName mn))) =
    (mkModuleName mn, occName)
nameToModFunc n = error $ "nameToModFunc failed because Name was not the right kind. " ++ show n

-- | predicate: event caused by file being added
isAdded :: Event -> Bool
isAdded (Added {}) = True
isAdded _          = False

-- | predicate: event caused by file being modified
isModified :: Event -> Bool
isModified (Modified {}) = True
isModified _             = False

-- | watch a single file for changes
watchFile :: WatchManager -> (FilePath, FilePath) -> IO () -> IO ()
watchFile wm (dir, file) action =
    watchDir wm dir
                 (\e -> filename (eventPath e) == file)
                 (\e -> if (isAdded e || isModified e)
                          then action
                          else return ())

-- | watch a bunch of files for changes
watchFiles :: WatchManager -> [FilePath] -> IO () -> IO ()
watchFiles wm fps action =
    do let pairs = Map.toList . Map.fromListWith (++) . map (\(x,y) -> (if x == mempty then "." else x,[y])) $ map splitFileName fps
       print pairs
       mapM_ watchFiles' pairs
    where
      splitFileName fp = (dirname fp, filename fp)
      watchFiles' :: (FilePath, [FilePath]) -> IO ()
      watchFiles' (dir, files) =
          watchDir wm dir
                   (\e -> filename (eventPath e) `elem` files)
                   (\e -> if (isAdded e || isModified e)
                          then action
                          else return ())

-- | wrapper for calling a Ghc action
--
-- defaults to 'HscAsm' and 'LinkInMemory'
withSession' :: Ghc a -> IO a
withSession' action =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscAsm
                                    , ghcLink   = LinkInMemory
                                    }
        action



------------------------------------------------------------------------------
-- PluginsHandle
------------------------------------------------------------------------------

data PluginsHandle = PluginsHandle
    { phWatchManager :: TMVar WatchManager
    , phSymMap         :: TMVar (Map TH.Name HValue)
    }

-- | create a new, empty 'PluginsHandle'
newPluginsHandle :: IO PluginsHandle
newPluginsHandle =
    PluginsHandle <$> (newTMVarIO =<< startManager) <*> newTMVarIO Map.empty

-- | set the list of modules that GHC should load
setTargets' :: [(ModuleName, String)] -> Ghc ()
setTargets' syms =
    do targets <- mapM (\(mod,_) -> (liftIO $ print $ moduleNameString mod) >> guessTarget (moduleNameSlashes mod) Nothing) syms
       setTargets targets
       return ()

-- | recompile and reload modified modules currently in the watch
-- list. Also update the watch list based on the new dependency graph.
--
-- FIXME: we probably need some form of semaphore here to protect use against multiple simultaneous calls
reload :: PluginsHandle
       -> [TH.Name]
       -> IO ()
reload ph newSyms =
    do m <- atomically $ takeTMVar (phSymMap ph)
       m' <- withSession' $ do
         let names = (Map.keys m) ++ newSyms
             syms = map nameToModFunc names
         setTargets' syms
         vals <- loadSyms syms
         updateWatches ph
         return $ Map.fromList $ zip names vals
       atomically $ putTMVar (phSymMap ph) m'

-- | look at the current module graph and update the list of watched
-- files accordingly.
updateWatches :: PluginsHandle
              -> Ghc ()
updateWatches ph =
    do wm <- liftIO $ do
         newWM <- startManager
         oldWM <- atomically $ do old <- takeTMVar (phWatchManager ph)
                                  putTMVar (phWatchManager ph) newWM
                                  return old
         stopManager oldWM
         return newWM
       modGraph <- getModuleGraph
       let files = catMaybes $ map (ml_hs_file . ms_location) modGraph
       liftIO $ do putStr "Now watching: "
                   print files
                   watchFiles wm (map fromString files) (reload ph [])

-- | load the modules+symbols
loadSyms :: [(ModuleName, String)] -> Ghc [HValue]
loadSyms syms =
    do res <- load LoadAllTargets

       -- Bringing the module into the context
       setContext (map (IIDecl . simpleImportDecl . fst) syms)

       let symNames = map (\(modName, symName) -> moduleNameString modName ++ "." ++ symName) syms
       liftIO $ print symNames
       mapM compileExpr symNames


-- | look up the symbol refered to by 'TH.Name' and call
-- 'unsafeCoerce' to cast it to type 'a'.
--
-- see also: 'lookupName'
unsafeLookupName :: TH.Name
               -> a
               -> PluginsHandle
               -> IO a
unsafeLookupName n _ ph =
    do sym <- atomically $ do m <- readTMVar (phSymMap ph)
                              case Map.lookup n m of
                                Nothing  -> error "Invalid name"
                                (Just s) -> return s
       return $ unsafeCoerce sym

-- | TH to safely lookup a symbol
--
-- generates a function like:
--
-- lookupName :: TH.Name -> PluginsHandle -> IO a
--
-- where the type 'a' is derived from the actual type of the symbol
-- refered to by 'Name' which must be in scope at compile time.
lookupName :: TH.Name -> ExpQ
lookupName name =
    appE (appE [| unsafeLookupName |] (lift name)) (varE name)
