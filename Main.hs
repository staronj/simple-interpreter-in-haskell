-- Jakub Staroń, 2017
module Main where

import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)
import qualified AST
import Data.Tree
import Control.Monad
import Control.Monad.Except
import FormatString
import Compile
import Data.Int

data Options = Options
 { optVerbose     :: Bool
 , optDumpAst        :: Bool
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
 , optDumpAst        = False
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
main = do
  args <- getArgs
  (options, filePath) <- compilerOpts args
  when (optVerbose options) $ putStrLn $ format "Reading file %0." [filePath]
  file <- readFile filePath
  when (optVerbose options) $ putStrLn "Building AST."
  let ast = AST.buildAST file
  ast <- case ast of
    Right ast -> (when (optDumpAst options) $ putStrLn $ AST.prettyPrint ast) >> return ast
    Left err -> fail err
  let program = compile ast
  program <- case program of
    Right program -> return program
    Left err -> fail err
  input <- getContents >>= (return.(map read).lines)
  let output = execute program input
  case output of
    Right output -> mapM_ print output
    Left (err, output) -> (mapM_ print output) >> (print err)
