-- Jakub Staroń, 2017
module Main where

import Data.Int
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.Tree

import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)

import qualified AST
import FormatString
import TypeCheck
import Compile


data Options = Options
 { optVerbose     :: Bool
 , optDumpAst        :: Bool
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
 , optDumpAst     = False
 }

{-
options :: [Opt.OptDescr (Options -> Options)]
options =
 [ Opt.Option ['v']     ["verbose"]
     (Opt.NoArg (\ opts -> opts { optVerbose = True }))
     "chatty output on stderr"
 , Opt.Option ['V','?'] ["version"]
     (Opt.NoArg (\ opts -> opts { optShowVersion = True }))
     "show version number"
 , Opt.Option ['o']     ["output"]
     (Opt.OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
             "FILE")
     "output FILE"
 , Opt.Option ['c']     []
     (Opt.OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
             "FILE")
     "input FILE"
 , Opt.Option ['L']     ["libdir"]
     (Opt.ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
     "library directory"
 ]
-}

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
main = liftM (maybe () id) . runMaybeT $ do
  args <- liftIO $ getArgs
  (options, filePath) <-  liftIO $ compilerOpts args
  when (optVerbose options) $ liftIO $ putStrLn $ format "Reading file %0." [filePath]
  file <-  liftIO $ readFile filePath
  when (optVerbose options) $ liftIO $ putStrLn "Building AST."
  let ast = AST.buildAST file
  ast <- case ast of
    Right ast -> (when (optDumpAst options) $ (liftIO $ putStrLn $ AST.prettyPrint ast) >> mzero) >> return ast
    Left err -> (liftIO $ putStrLn err) >> mzero
  when (optVerbose options) $ liftIO $ putStrLn "Type checking."
  ast <- case typeCheck ast of
    Right ast -> return ast
    Left err -> (liftIO $ putStrLn err) >> mzero
  when (optVerbose options) $ liftIO $ putStrLn "Compiling into lambda."
  let program = compile ast
  program <- case program of
    Right program -> return program
    Left err -> (liftIO $ putStrLn err) >> mzero
  input <- liftIO $ getContents >>= (return.(map read).lines)
  when (optVerbose options) $ liftIO $ putStrLn "Executing."
  let output = execute program input
  case output of
    Right output -> liftIO $ mapM_ print output
    Left (err, output) -> liftIO $ (mapM_ print output) >> (print err)
