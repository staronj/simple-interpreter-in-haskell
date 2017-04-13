-- automatically generated by BNF Converter
module Main where

import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer


valid :: (Print a, Show a) => ParseFun a -> String -> IO ()
valid parse str = let tokens = myLLexer str in case parse tokens of
           Bad msg  -> do putStrLn $ "ERROR expected valid, is invalid  for \"" ++ str ++ "\""
           Ok  tree -> do return () -- putStrLn $ "OK                                for \"" ++ str ++ "\"" -- ++ ", tree: \t\t\t" ++  show tree

invalid :: (Print a, Show a) => ParseFun a -> String -> IO ()
invalid parse str = let tokens = myLLexer str in case parse tokens of
           Bad mgs  -> do return () -- putStrLn $ "OK                                for \"" ++ str ++ "\""
           Ok  tree -> do putStrLn $ "ERROR expected invalid, is valid  for \"" ++ str ++ "\"" -- ++ ", tree: \t\t\t" ++  show tree

test = 
  [
    valid pExpr       "1",
    valid pExpr       "(1)",
    valid pExpr       "1 + 2",
    valid pExpr       "1 * 2",
    valid pExpr       "1 + 2 + 3",
    valid pExpr       "1 * 2 + 33",
    valid pExpr       "1 + 2 * 3",
    invalid pExpr     "1 == 2 == 3",
    valid pExpr       "(1 == 1) == 3",
    valid pExpr       "true",
    valid pExpr       "false",
    valid pExpr       "a",
    valid pExpr       "1 + a",
    valid pExpr       "&a",
    valid pExpr       "& mut a",

    valid pExpr       "[]",
    invalid pExpr     "[,]",
    valid pExpr       "[1,]",
    valid pExpr       "[1,2]",
    invalid pExpr     "[1,2,]",
    valid pExpr       "[1,2,3]",
    invalid pExpr     "[1,2,3,]",

    valid pExpr       "[1 ; 2]",
    valid pExpr       "[1 + 1 ; 2]",
    invalid pExpr     "[1 ; 2 + 2]",
    invalid pExpr     "[1 2 + 2]",
    valid pExpr       "[() ; 123]",
    invalid pExpr     "[() ; -123]",

    valid pExpr       "[1..2]",
    valid pExpr       "[0..0]",
    invalid pExpr     "[1...1]",
    invalid pExpr     "[...1]",
    invalid pExpr     "[a..1]",
    invalid pExpr     "[false..true]",

    valid pExpr       "()",
    invalid pExpr     "(,)",
    valid pExpr       "(1,)",
    valid pExpr       "(1,2)",
    invalid pExpr     "(1,2,)",
    valid pExpr       "(1,2,3)",
    invalid pExpr     "(1,2,3,)",

    valid pExpr       "foo()",
    invalid pExpr     "foo(,)",
    valid pExpr       "foo(1)",
    invalid pExpr     "foo(1,)",
    valid pExpr       "foo(1,2)",
    invalid pExpr     "foo(1,2,)",
    invalid pExpr     "foo(i32)",
    invalid pExpr     "foo(i32)",
    invalid pExpr     "foo{i32}",
    valid pExpr       "foo(bar(1))",
    
    valid pExpr       "a[1]",
    invalid pExpr     "a[]",
    invalid pExpr     "a[1,]",
    invalid pExpr     "a[1,2]",
    valid pExpr       "(1 + 1)[1 + 1]",
    
    valid pExpr       "a.0",
    valid pExpr       "(1+1,).0",
    valid pExpr       "(1+1, 2+2).0",
    invalid pExpr     "a.(1 + 1)",
    invalid pExpr     "a.(1)",
    
    valid pExpr       "{ }",
    valid pExpr       "a = b",
    valid pExpr       "a = b = c",
    valid pExpr       "(a = b) = c",
    valid pExpr       "a = (b = c)",

    invalid pType     "1",
    valid pType       "i32",
    valid pType       "bool",
    valid pType       "()",
    invalid pType     "(,)",
    valid pType       "(i32,)",
    invalid pType     "(i32)",
    valid pType       "(i32, bool)",
    invalid pType     "(i32, bool, )",
    valid pType       "(i32, bool, i32)",
    invalid pType     "(i32, bool, i32, )",

    valid pLetDecl    "let e = 32 ;",
    invalid pLetDecl  "let e = 32 ",
    valid pLetDecl    "let _ = 1 + 1 ; ",
    valid pLetDecl    "let (_, (a, b, (_,))) = 1 ; ",
    valid pLetDecl    "let (a,) = 1 ; ",
    invalid pLetDecl  "let (,) = 1 ; ",
    invalid pLetDecl  "let (a) = 1 ; ",
    valid pLetDecl    "let (a, b) = 1 ; ",
    invalid pLetDecl  "let (a, b,) = 1 ; ",
    valid pLetDecl    "let (a, b, c) = 1 ; ",
    invalid pLetDecl  "let (a, b, c,) = 1 ; ",

    valid pParameter    "n : i32",
    invalid pParameter  "n",
    invalid pParameter  " : i32",
    invalid pParameter  "n : 1",


    valid pFunDecl    "fn foo() { }",
    valid pFunDecl    "fn foo() -> i32 { }",
    valid pFunDecl    "fn foo()->i32 { }",
    valid pFunDecl    "fn foo(n : i32) { }",
    invalid pFunDecl  "fn foo(n : i32,) { }",
    valid pFunDecl    "fn foo(a : i32, b : bool) { }",
    invalid pFunDecl  "fn foo(a : i32, b : bool,) { }",
    valid pFunDecl    "fn foo(a : i32, b : bool, c : (i32, i32)) { }",
    invalid pFunDecl  "fn foo() ",
    invalid pFunDecl  "fn foo() { ",

    valid pStmt       "break;",
    invalid pStmt     "break",
    valid pStmt       "break;",
    invalid pStmt     "continue",
    valid pStmt       "1 + 1;",
    invalid pStmt     "1 + 1",
    invalid pStmt     "1; 1;",
    valid pStmt       "a = 1 ;",
    valid pStmt       "foo(1,2,3).4 = (2 + 2) ;",
    valid pStmt       "id = { } ;",

    valid pBlock      "{ }",
    valid pBlock      "{ 1 }",
    valid pBlock      "{ 1 ; }",
    valid pBlock      "{ 1 ; 1 }",
    invalid pBlock    "{ 1 1 }",

    valid pIfStmt     "if 1 + 1 { }",
    valid pIfStmt     "if a { }",
    invalid pIfStmt   "if a b",
    invalid pIfStmt   "if { } b",
    invalid pIfStmt   "if a { } else { }",
    valid pStmt       "if a { }",
    invalid pExpr     "if a { }",

    valid pIfElse     "if a { } else { }",
    invalid pIfElse   "if a { } else",
    invalid pIfElse   "if if { }",
    invalid pIfElse   "if else { }",
    invalid pIfElse   "if { } b else { }",
    invalid pIfElse   "if a { } { }",
    valid pStmt       "if a { } else { }",
    valid pExpr       "if a { } else { }",


    valid pLoop       "loop { }",
    valid pLoop       "while a { }",
    valid pLoop       "for a in [1, 2, 3] { }",
    invalid pLoop     "for in in [1, 2, 3] { }",
    valid pLoop       "for a in 1 .. 2 { }",
    valid pLoop       "for a in (1+1) .. (2+2) { }",

    valid pBlock      "{ }",
    valid pBlock      "{ 1 }",
    invalid pBlock    "{ 1 2 }",
    valid pBlock      "{ 1; 2 }",

    valid pProgram    "fn foo() { } fn bar() { } fn main() { }"
  ]

main :: IO ()
main = sequence_ test





