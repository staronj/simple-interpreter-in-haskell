module Main where

import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)
import Control.Monad.Except
import qualified AST;


data Options = Options
 { optVerbose     :: Bool
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
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
  file <- readFile filePath
  let ast  = AST.buildAST file
  print ast