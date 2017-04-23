-- Jakub Staroń, 2017
module Main where

import Control.Monad
import Data.Either

import FormatString
import AST
import TypeCheck

left :: Either a b -> a
left either = case  either of
    Left a -> a
    Right b -> undefined

right :: Either a b -> b
right either = case  either of
    Left a -> undefined
    Right b -> b

test :: Bool -> String -> String -> IO ()
test shouldTypeCheck message code = do
    let ast = AST.buildAST code
    when (isLeft ast) $ fail $ format "Test \"%0\" failed to parse, error is\n%1" [message, left ast]
    let typeChecked = typeCheck $ right ast
    case typeChecked of
        Left err -> when shouldTypeCheck $ fail $ format "Test \"%0\" should type-check but it don't. Error is \"%1\"." [message, err]
        Right _ -> unless shouldTypeCheck $ fail $ format "Test \"%0\" should not type-check but it does." [message]
tests =
  [
    test True "empty-main"
        "fn main() { }",

    test False "main-wrong-number-of-arguments"
        "fn main(a : i32) { }",

    test False "main-wrong-declared-return-type"
        "fn main() -> i32 { 42 }",

    test False "main-wrong-return-type"
        "fn main() { 42 }",

    test False "main-not-declared"
        "fn foo() { }",

    test False "main-not-declared-ex"
        "fn foo() { fn main() { } }",

    test True "function-declared"
        "fn foo() { } fn main() { foo(); }",

    test False "function-not-declared"
        "fn main() { foo(); }",

    test False "if-wrong-block-type"
        "fn main() { if true { 42 } }",

    test False "if-wrong-condition-type"
        "fn main() { if 42 { } }",

    test True "if-minimal-example"
        "fn main() { if true { } }",

    test True "if-else-minimal-example"
        "fn main() { if true { } else { } }",

    test True "if-else-incompatible-types"
        "fn main() { { if true { true } else { 42 } }; }"
  ]

main :: IO ()
main = sequence_ tests





