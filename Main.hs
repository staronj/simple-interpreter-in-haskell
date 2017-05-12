-- Jakub Staroń, 2017

module Main where

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Except

import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)

import qualified AST.Print as AST
import qualified AST.Build as AST
import FormatString (format)
import TypeCheck (typeCheck)
import Compile (compile, execute)
import qualified Intermediate.Build as Intermediate

data Options = Options
 { optVerbose     :: Bool
 , optDumpAst        :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions    = Options
 { optVerbose     = False
 , optDumpAst     = False
 }

options :: [Opt.OptDescr (Options -> Options)]
options =
 [
   Opt.Option ['v']     ["verbose"]
     (Opt.NoArg (\ opts -> opts { optVerbose = True }))
     "verbose output on stderr"
  ,Opt.Option ['d']     ["ast-dump"]
     (Opt.NoArg (\ opts -> opts { optDumpAst = True }))
     "instead of interpreting program dump abstract syntax tree (before type checking)"
 ]

compilerOpts :: [String] -> IO (Options, String)
compilerOpts argv =
   case Opt.getOpt Opt.RequireOrder options argv of
      (o,[file],[]  ) -> return (foldl (flip id) defaultOptions o, file)
      (_,_,errs) -> ioError (userError (concat errs ++ Opt.usageInfo header options))
  where header = "Usage: interpret [OPTIONS...] file"

main :: IO ()
main = fmap (fromMaybe ()) $ runMaybeT $ do
  args <- liftIO getArgs
  (options, filePath) <-  liftIO $ compilerOpts args
  when (optVerbose options) $ liftIO $ putStrLn $ format "Reading file %0." [filePath]
  file <-  liftIO $ readFile filePath
  when (optVerbose options) $ liftIO $ putStrLn "Building AST."
  let ast = AST.buildAST file
  ast <- case ast of
    Right ast -> when (optDumpAst options) (liftIO (putStrLn $ AST.prettyPrint ast) >> mzero) >> return ast
    Left err -> liftIO (putStrLn err) >> mzero
  when (optVerbose options) $ liftIO $ putStrLn "Type checking."
  ast <- case typeCheck ast of
    Right ast -> return ast
    Left err -> liftIO (putStrLn err) >> mzero
  when (optVerbose options) $ liftIO $ putStrLn "Compiling into lambda."
  let intermediate = Intermediate.fromAST ast
  let program = compile intermediate
  input <- liftIO $ fmap (map read . lines) getContents
  when (optVerbose options) $ liftIO $ putStrLn "Executing."
  let output = execute program input
  liftIO $ mapM_ print (fst output)
  liftIO $ maybe (return ()) print (snd output)
