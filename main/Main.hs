module Main where

import System.Console.Haskeline
import System.IO
import System.Environment
import System.Exit
import System.FilePath ((</>), addTrailingPathSeparator)

import Data.Maybe
import Data.Version
import Control.Monad.Trans.Error ( ErrorT(..) )
import Control.Monad.Trans.State.Strict ( execStateT, get, put )
import Control.Monad ( when )

import Idris.Core.TT
import Idris.Core.Typecheck
import Idris.Core.Evaluate
import Idris.Core.Constraints

import Idris.AbsSyntax
import Idris.Parser
import Idris.REPL
import Idris.ElabDecls
import Idris.Primitives
import Idris.Imports
import Idris.Error
import Idris.CmdOptions
import IRTS.CodegenCommon (OutputType(..))

import Idris.Output (iputStrLn, pshow)

import IRTS.System ( getLibFlags, getIdrisLibDir, getIncFlags )

import Util.DynamicLinker

import Pkg.Package

import Paths_idris

-- Main program reads command line options, parses the main program, and gets
-- on with the REPL.

main :: IO ()
main = do opts <- runArgParser
          runMain (runIdris opts)

-- idris compiler main
idrisc :: [Opt] -> Idris ()
idrisc opts = do
       let verbose = Verbose `elem` opts
       let importdirs = opt getImportDir opts
       let inputs = opt getFile opts
       let ibcsubdir = opt getIBCSubDir opts
       let output = opt getOutput opts
       let cgn = case opt getCodegen opts of
                   [] -> Via "c"
                   xs -> last xs
       let outty = case opt getOutputTy opts of
                     [] -> Executable
                     xs -> last xs
       let pkgdirs = opt getPkgDir opts
       setVerbose verbose
       setCodegen cgn
       setOutputTy outty
       setWidth InfinitelyWide
       case ibcsubdir of
         [] -> setIBCSubDir ""
         (d:_) -> setIBCSubDir d
       setImportDirs importdirs

       mapM_ addPkgDir $ (if NoBasePkgs `elem` opts then [] else ["prelude", "base"]) ++ pkgdirs
       elabPrims
       when (not (NoBuiltins `elem` opts)) $ do x <- loadModule "Builtins"
                                                addAutoImport "Builtins"
                                                return ()
       when (not (NoPrelude `elem` opts)) $ do x <- loadModule "Prelude"
                                               addAutoImport "Prelude"
                                               return ()

       loadInputs inputs Nothing

       ok <- noErrors
       when ok $ case output of
                    [] -> return ()
                    (o:_) -> idrisCatch (process "" (Compile cgn o))
                               (\e -> do ist <- getIState ; iputStrLn $ pshow ist e)

  where
    addPkgDir :: String -> Idris ()
    addPkgDir p = do ddir <- runIO $ getDataDir
                     addImportDir (ddir </> p)
                     addIBC (IBCImportDir (ddir </> p))


runIdris :: [Opt] -> Idris ()
runIdris opts = do
       when (ShowIncs `elem` opts) $ runIO showIncs
       when (ShowLibs `elem` opts) $ runIO showLibs
       when (ShowLibdir `elem` opts) $ runIO showLibdir
       case opt getClient opts of
           []    -> return ()
           (c:_) -> do setVerbose False
                       setQuiet True
                       runIO $ runClient (getPort opts) c
                       runIO $ exitWith ExitSuccess
       case opt getPkgCheck opts of
           [] -> return ()
           fs -> do runIO $ mapM_ (checkPkg (WarnOnly `elem` opts) True) fs
                    runIO $ exitWith ExitSuccess
       case opt getPkgClean opts of
           [] -> return ()
           fs -> do runIO $ mapM_ cleanPkg fs
                    runIO $ exitWith ExitSuccess
       case opt getPkgMkDoc opts of                -- IdrisDoc
           [] -> return ()
           fs -> do runIO $ mapM_ documentPkg fs
                    runIO $ exitWith ExitSuccess
       case opt getPkgTest opts of
           [] -> return ()
           fs -> do runIO $ mapM_ testPkg fs
                    runIO $ exitWith ExitSuccess
       if (opt getOutput opts == [])
         then case opt getPkg opts of
             [] -> case opt getPkgREPL opts of
                        [] -> idrisMain opts
                        [f] -> replPkg f
                        _ -> ifail "Too many packages"
             fs -> runIO $ mapM_ (buildPkg (WarnOnly `elem` opts)) fs
         else idrisc opts

showver :: IO b
showver = do putStrLn $ "Idris version " ++ ver
             exitWith ExitSuccess

showLibs :: IO b
showLibs = do libFlags <- getLibFlags
              putStrLn libFlags
              exitWith ExitSuccess

showLibdir :: IO b
showLibdir = do dir <- getIdrisLibDir
                putStrLn dir
                exitWith ExitSuccess

showIncs :: IO b
showIncs = do incFlags <- getIncFlags
              putStrLn incFlags
              exitWith ExitSuccess
